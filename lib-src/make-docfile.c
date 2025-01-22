/* Generate doc-string file for GNU Emacs from source files.

Copyright (C) 1985-1986, 1992-1994, 1997, 1999-2024 Free Software
Foundation, Inc.

This file is part of GNU Emacs.

GNU Emacs is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or (at
your option) any later version.

GNU Emacs is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.  */


/* The arguments given to this program are all the C files
of GNU Emacs.  .c files are allowed.
A .o file can also be specified; the .c file it was made from is used.
This helps the makefile pass the correct list of files.
Option -d DIR means change to DIR before looking for files.

The results, which go to standard output or to a file
specified with -a or -o (-a to append, -o to start from nothing),
are entries containing function or variable names and their documentation.
Each entry starts with a ^_ character.
Then comes F for a function or V for a variable.
Then comes the function or variable name, terminated with a newline.
Then comes the documentation for that function or variable.
*/

#include <config.h>

#include <stdarg.h>
#include <stddef.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

#include <attribute.h>
#include <binary-io.h>
#include <c-ctype.h>
#include <intprops.h>
#include <min-max.h>
#include <unlocked-io.h>

#ifdef WINDOWSNT
/* Defined to be sys_fopen in ms-w32.h, but only #ifdef emacs, so this
is really just insurance.  */
#undef fopen
#include <direct.h>
#endif /* WINDOWSNT */

#ifdef WINDOWSNT
/* Defined to be sys_chdir in ms-w32.h, but only #ifdef emacs, so this
is really just insurance.

Similarly, msdos defines this as sys_chdir, but we're not linking with the
file where that function is defined.  */
#undef chdir
#endif /* not WINDOWSNT */

#include <unistd.h>

static void scan_c_stream(FILE *infile);

/* Name this program was invoked with.  */
static char *progname;

/* True if this invocation is generating globals.h.  */
static bool generate_globals;

/* Print error message.  Args are like vprintf.  */

static void ATTRIBUTE_FORMAT_PRINTF(1, 0) verror(char const *m, va_list ap) {
	fprintf(stderr, "%s: ", progname);
	vfprintf(stderr, m, ap);
	fprintf(stderr, "\n");
}

/* Print error message.  Args are like printf.  */

static void ATTRIBUTE_FORMAT_PRINTF(1, 2) error(char const *m, ...) {
	va_list ap;
	va_start(ap, m);
	verror(m, ap);
	va_end(ap);
}

/* Print error message and exit.  Args are like printf.  */

static _Noreturn void ATTRIBUTE_FORMAT_PRINTF(1, 2) fatal(char const *m, ...) {
	va_list ap;
	va_start(ap, m);
	verror(m, ap);
	va_end(ap);
	exit(EXIT_FAILURE);
}

static _Noreturn void memory_exhausted (void) { fatal ("virtual memory exhausted"); }

/* Like malloc but get fatal error if memory is exhausted.  */

static void *ATTRIBUTE_MALLOC xmalloc(ptrdiff_t size) {
	void *result = malloc(size);
	if (result == NULL) memory_exhausted();
	return result;
}

/* Like realloc but get fatal error if memory is exhausted.  */

static void *xrealloc(void *arg, ptrdiff_t size) {
	void *result = realloc(arg, size);
	if (result == NULL) memory_exhausted();
	return result;
}


static void close_emacs_globals(ptrdiff_t num_symbols) {
	printf(
		(
			"};\n"
			"extern struct emacs_globals globals;\n"
			"\n"
			"extern struct Lisp_Symbol lispsym[%td];\n"
			"#ifdef DEFINE_SYMBOLS\n"
			"struct Lisp_Symbol lispsym[%td];\n"
			"#endif\n"
		),
		num_symbols,
		num_symbols
	);
}


// The types of globals. These are sorted roughly in decreasing alignment
// order to avoid allocation gaps, except that symbols and functions are last.
enum global_type {
	INVALID,
	LISP_OBJECT,
	EMACS_INTEGER,
	BOOLEAN,
	SYMBOL,
	FUNCTION
};

// A single global.
struct global {
	enum global_type type;
	char *name;
	int flags;
	union {
		int value;
		char const *svalue;
	} v;
};

// Bit values for FLAGS field from the above. Applied for DEFUNs only.
enum { DEFUN_noreturn = 1, DEFUN_const = 2 };

// All the variable names we saw while scanning C sources in `-g' mode.
static ptrdiff_t num_globals;
static ptrdiff_t num_globals_allocated;
static struct global *globals;


static int compare_globals(void const *a, void const *b) {
	struct global const *ga = a, *gb = b;

	if (ga->type != gb->type)
		return ga->type - gb->type;

	// Consider "nil" to be the least, so that iQnil is zero.
	// That way, Qnil's internal representation is zero, which is a bit faster.
	// Similarly, consider "t" to be the second-least, and so forth.
	if (ga->type == SYMBOL) {
		// Common symbols in decreasing popularity order.
		static char const commonsym[][8] = {"nil", "t", "unbound", "error", "lambda"};
		int ncommonsym = sizeof commonsym / sizeof *commonsym;
		int ai = ncommonsym, bi = ncommonsym;
		for (int i = 0; i < ncommonsym; i++) {
			if (ga->name[0] == 'Q' && strcmp(ga->name + 1, commonsym[i]) == 0)
				ai = i;
			if (gb->name[0] == 'Q' && strcmp(gb->name + 1, commonsym[i]) == 0)
				bi = i;
		}
		if (!(ai == ncommonsym && bi == ncommonsym))
			return ai - bi;
    }

	return strcmp(ga->name, gb->name);
}


int main(int argc, char **argv) {
	progname = argv[0];

	int i = 1;

	// If first two args are -o FILE, output to FILE.
	if (argc > 2 && !strcmp(argv[1], "-o")) {
		if (!freopen(argv[2], "w", stdout)) {
			perror(argv[2]);
			return EXIT_FAILURE;
		}
		i += 2;
    }
	if (argc > i + 1 && !strcmp(argv[i], "-a")) {
		if (!freopen(argv[i + 1], "a", stdout)) {
			perror(argv[i + 1]);
			return EXIT_FAILURE;
		}
		i += 2;
    }
	if (argc > i + 1 && !strcmp(argv[i], "-d")) {
		if (chdir(argv[i + 1]) != 0) {
			perror(argv[i + 1]);
			return EXIT_FAILURE;
		}
		i += 2;
    }
	if (argc > i && !strcmp(argv[i], "-g")) {
		generate_globals = true;
		++i;
    }

	set_binary_mode(fileno(stdout), O_BINARY);

	if (generate_globals) {
		puts("/* This file was auto-generated by make-docfile.  */");
		puts("/* DO NOT EDIT.  */");
		puts("struct emacs_globals {");
	}

	if (i >= argc)
		scan_c_stream(stdin);
	else
		for (int first_infile = i; i < argc; ++i) {
			int j = first_infile;
			// Don't process one file twice.
			while (j < i && strcmp(argv[i], argv[j])) ++j;
			// Read file argv[i] and output its doc strings to stdout.
			if (j == i) {
				char *filename = argv[i];

				if (!generate_globals) {
					// Add a source file name boundary marker in the output file.
					char *basename = filename;
					for (char *tmp = filename; *tmp; ++tmp)
						if (IS_DIRECTORY_SEP(*tmp))
							basename = tmp + 1;
					printf("\037S%s\n", basename);
				}

				// Read through a c file.
				// If a .o file is named, the corresponding .c or .m file is read instead.
				// Looks for DEFUN constructs such as are defined in ../src/lisp.h.

				char const extension = filename[strlen(filename) - 1];

				if (extension == 'o')
					filename[strlen(filename) - 1] = 'c';

				FILE *infile = fopen(filename, "r");

				if (infile == NULL && extension == 'o') {
					// Try .m.
					filename[strlen(filename) - 1] = 'm';
					infile = fopen(filename, "r");
					if (infile == NULL)
						filename[strlen(filename) - 1] = 'c'; // Don't confuse people.
			    }

				if (infile == NULL) {
					perror(filename);
					exit(EXIT_FAILURE);
			    }

				// Reset extension to be able to detect duplicate files.
				filename[strlen(filename) - 1] = extension;
				scan_c_stream(infile);
			}
		}

	if (generate_globals) {
		ptrdiff_t symnum = 0, num_symbols = 0;
		bool seen_defun = false;

		qsort(globals, num_globals, sizeof (struct global), compare_globals);

		{
			ptrdiff_t j = 0;
			for (ptrdiff_t i = 0; i < num_globals; ++i) {
				while (
					i + 1 < num_globals
					&& !strcmp(globals[i].name, globals[i + 1].name)
				) {
					if (
						globals[i].type == FUNCTION
						&& globals[i].v.value != globals[i + 1].v.value
					)
						error(
							"function '%s' defined twice with differing signatures",
							globals[i].name
						);
					free(globals[i].name);
					++i;
				}
				num_symbols += globals[i].type == SYMBOL;
				globals[j++] = globals[i];
		    }
			num_globals = j;
		}

		for (ptrdiff_t i = 0; i < num_globals; ++i) {
			char const *type = 0;

			switch (globals[i].type) {
				case EMACS_INTEGER:
					type = "intmax_t";
					break;
				case BOOLEAN:
					type = "bool";
					break;
				case LISP_OBJECT:
					type = "Lisp_Object";
					break;
				case SYMBOL:
				case FUNCTION:
					if (!seen_defun) {
						close_emacs_globals(num_symbols);
						putchar('\n');
						seen_defun = true;
					}
					break;
				default: fatal("not a recognized DEFVAR_");
			}

			if (type) {
				printf("  %s f_%s;\n", type, globals[i].name);
				printf("#define %s globals.f_%s\n", globals[i].name, globals[i].name);
			} else if (globals[i].type == SYMBOL)
				printf(
					"#define i%s %td\nDEFINE_LISP_SYMBOL (%s)\n",
					globals[i].name, symnum++, globals[i].name
				);
			else {
				if (globals[i].flags & DEFUN_noreturn)
					fputs("_Noreturn ", stdout);

				printf("EXFUN (%s, ", globals[i].name);
				if (globals[i].v.value == -1)
					fputs("MANY", stdout);
				else if (globals[i].v.value == -2)
					fputs("UNEVALLED", stdout);
				else
					printf("%d", globals[i].v.value);
				putchar(')');

				if (globals[i].flags & DEFUN_noreturn)
					fputs(" ATTRIBUTE_COLD", stdout);
				if (globals[i].flags & DEFUN_const)
					fputs(" ATTRIBUTE_CONST", stdout);

				puts(";");
			}
	    }

		if (!seen_defun) close_emacs_globals(num_symbols);

		puts("#ifdef DEFINE_SYMBOLS");
		puts("static char const *const defsym_name[] = {");
		for (ptrdiff_t i = 0; i < num_globals; i++)
			if (globals[i].type == SYMBOL)
				printf("\t\"%s\",\n", globals[i].v.svalue);
		puts("};");
		puts("#endif");

		puts("#define Qnil builtin_lisp_symbol (0)");
		puts("#if DEFINE_NON_NIL_Q_SYMBOL_MACROS");
		num_symbols = 0;
		for (ptrdiff_t i = 0; i < num_globals; i++)
			if (globals[i].type == SYMBOL && num_symbols++)
				printf(
					"# define %s builtin_lisp_symbol (%td)\n",
					globals[i].name, num_symbols - 1
				);
		puts("#endif");
	}

	if (ferror(stdout) || fclose(stdout)) fatal("write error");

	return EXIT_SUCCESS;
}


static char input_buffer[128];

// Some state during the execution of `read_c_string_or_comment'.
struct rcsoc_state {
	/* A count of spaces and newlines that have been read, but not output.  */
	intmax_t pending_spaces, pending_newlines;

	/* Where we're reading from.  */
	FILE *in_file;

	/* If non-zero, a buffer into which to copy characters.  */
	char *buf_ptr;
	/* If non-zero, a file into which to copy characters.  */
	FILE *out_file;

	/* A keyword we look for at the beginning of lines.  If found, it is
	not copied, and SAW_KEYWORD is set to true.  */
	const char *keyword;
	/* The current point we've reached in an occurrence of KEYWORD in
	the input stream.  */
	const char *cur_keyword_ptr;
	/* Set to true if we saw an occurrence of KEYWORD.  */
	bool saw_keyword;
};

// Output CH to the file or buffer in STATE.
// Any pending newlines or spaces are output first.
static void put_char(char const ch, struct rcsoc_state *const state) {
	char out_ch;
	do {
		if (state->pending_newlines > 0) {
			--state->pending_newlines;
			out_ch = '\n';
		} else if (state->pending_spaces > 0) {
			--state->pending_spaces;
			out_ch = ' ';
		} else out_ch = ch;

		if (state->out_file)
			putc(out_ch, state->out_file);
		if (state->buf_ptr)
			*state->buf_ptr++ = out_ch;
    } while (out_ch != ch);
}

// If in the middle of scanning a keyword, continue scanning with
// character CH, otherwise output CH to the file or buffer in STATE.
// Any pending newlines or spaces are output first, as well as any
// previously scanned characters that were thought to be part of a
// keyword, but were in fact not.
static void scan_keyword_or_put_char(char const ch, struct rcsoc_state *const state) {
	if (
		state->keyword
		&& *state->cur_keyword_ptr == ch
		&& (
			state->cur_keyword_ptr > state->keyword
			|| state->pending_newlines > 0
		)
	) {
		// We might be looking at STATE->keyword at some point.
		// Keep looking until we know for sure.
		if (*++state->cur_keyword_ptr == '\0') {
			// Saw the whole keyword.

			state->saw_keyword = true;

			// Reset the scanning pointer.
			state->cur_keyword_ptr = state->keyword;

			// Canonicalize whitespace preceding a usage string.
			state->pending_newlines = 2;
			state->pending_spaces = 0;

			// Skip any spaces and newlines between the keyword and the usage string.
			int c;
			do c = getc(state->in_file); while (c == ' ' || c == '\n');

			// Output the open-paren we just read.
			if (c != '(') fatal("Missing '(' after keyword");
			put_char(c, state);

			// Skip the function name and replace it with `fn'.
			do {
				c = getc(state->in_file);
				if (c == EOF) fatal("Unexpected EOF after keyword");
			} while (c != ' ' && c != ')');

			put_char('f', state);
			put_char('n', state);

			// Put back the last character.
			ungetc(c, state->in_file);
		}
    } else {
		if (state->keyword && state->cur_keyword_ptr > state->keyword) {
			// We scanned the beginning of a potential usage keyword,
			// but it was a false alarm. Output the part we scanned.
			for (char const *p = state->keyword; p < state->cur_keyword_ptr; ++p)
				put_char(*p, state);
			state->cur_keyword_ptr = state->keyword;
		}
		put_char(ch, state);
    }
}

// Skip a C string or C-style comment from INFILE, and return the
// byte that follows, or EOF. COMMENT means skip a comment.
// If PRINTFLAG is positive, output string contents to stdout.
// If it is negative, store contents in buf.
// Convert escape sequences \n and \t to newline and tab; discard \ followed by newline.
// If SAW_USAGE is non-null, then any occurrences of the string "usage:"
// at the beginning of a line will be removed, and *SAW_USAGE set to
// true if any were encountered.

static char read_c_string_or_comment(
	FILE *const file, int const printflag, bool const comment, bool *const saw_usage
) {
	struct rcsoc_state state;

	state.in_file = file;
	state.buf_ptr = printflag < 0 ? input_buffer : 0;
	state.out_file = printflag > 0 ? stdout : 0;
	state.pending_spaces = 0;
	state.pending_newlines = 0;
	state.keyword = saw_usage ? "usage:" : 0;
	state.cur_keyword_ptr = state.keyword;
	state.saw_keyword = false;

	char c = getc(file);
	if (comment) while (c_isspace(c)) c = getc(file);

	while (c != EOF) {
		while (c != EOF && c != (comment ? '*' : '"')) {
			if (c == '\\') {
				c = getc(file);
				switch (c) {
					case '\n':
						c = getc(file);
						continue;
					case 'n': c = '\n'; break;
					case 't': c = '\t'; break;
				}
			}

			if (c == ' ') ++state.pending_spaces;
			else if (c == '\n') {
				++state.pending_newlines;
				state.pending_spaces = 0;
			} else scan_keyword_or_put_char(c, &state);

			c = getc(file);
		}

		if (c != EOF) c = getc(file);

		if (comment) {
			if (c == '/') {
				c = getc(file);
				break;
			}
			scan_keyword_or_put_char('*', &state);
		} else if (c != '"') break;
		// If we had a "", concatenate the two strings.
		else c = getc(file);
    }

	if (printflag < 0) *state.buf_ptr = 0;

	if (saw_usage) *saw_usage = state.saw_keyword;

	return c;
}

static struct global *add_global(
	enum global_type type, char const *name, int value, char const *svalue
) {
	// Ignore the one non-symbol that can occur.
	if (strcmp(name, "...")) {
		if (num_globals == num_globals_allocated) {
			ptrdiff_t num_globals_max = (min(PTRDIFF_MAX, SIZE_MAX) / sizeof *globals);
			if (num_globals_allocated == num_globals_max)
				memory_exhausted ();
			if (num_globals_allocated < num_globals_max / 2)
				num_globals_allocated = 2 * num_globals_allocated + 1;
			else
				num_globals_allocated = num_globals_max;
			globals = xrealloc(globals, num_globals_allocated * sizeof *globals);
		}

		++num_globals;

		ptrdiff_t namesize = strlen(name) + 1;
		char *const buf = xmalloc(namesize + (svalue ? strlen(svalue) + 1 : 0));
		globals[num_globals - 1].type = type;
		globals[num_globals - 1].name = strcpy(buf, name);
		if (svalue)
			globals[num_globals - 1].v.svalue = strcpy(buf + namesize, svalue);
		else
			globals[num_globals - 1].v.value = value;
		globals[num_globals - 1].flags = 0;
		return globals + num_globals - 1;
    }
	return NULL;
}

// Skip line, so that next getc call will return first char from this next line.
static void skip_line(FILE *const file) {
	char c;
	do c = getc(file); while (c != '\n' && c != EOF);
}

static void scan_c_stream(FILE *const file) {
	int commas, minargs, maxargs;

	while (!feof(file)) {
		bool doc_keyword = false;
		bool defunflag = false;
		bool defvarperbufferflag = false;
		bool defvarflag = false;
		enum global_type type = INVALID;
		static char name[sizeof input_buffer];
		char c;

		while (c_isspace(c = getc(file)));

		if (c == EOF) break;

		// Move to the next line for the next loop,
		// unless we are already at the start of the line.
		#define MAYBE_SKIP_LINE {			\
			if (c != '\n') skip_line(file);	\
			continue;						\
		}

		#define PROCESS_NEXT_CHAR(valid_char)		\
			c = getc(file);							\
			if (c != valid_char) MAYBE_SKIP_LINE

		if (c != 'D') MAYBE_SKIP_LINE
		PROCESS_NEXT_CHAR('E')
		PROCESS_NEXT_CHAR('F')

		c = getc(file);
		switch (c) {
			case 'S':
				PROCESS_NEXT_CHAR('Y')
				PROCESS_NEXT_CHAR('M')
				type = SYMBOL;
				c = getc(file);
				break;
			case 'V':
				PROCESS_NEXT_CHAR('A')
				PROCESS_NEXT_CHAR('R')
				PROCESS_NEXT_CHAR('_')
				defvarflag = true;
				c = getc(file);
				if (generate_globals)
					switch (c) {
						case 'I': type = EMACS_INTEGER; break;
						case 'L': type = LISP_OBJECT; break;
						case 'B': type = BOOLEAN; break;
						case 'P': skip_line(file); continue;
						default: MAYBE_SKIP_LINE
					}
				defvarperbufferflag = c == 'P';
				c = getc(file);
				// We need to distinguish between DEFVAR_BOOL and DEFVAR_BUFFER_DEFAULTS.
				if (type == BOOLEAN && c != 'O') MAYBE_SKIP_LINE
				// Skip to the char after macro name.
				while (('A' <= c && c <= 'Z') || c == '_') c = getc(file);
				break;
			case 'U':
				PROCESS_NEXT_CHAR('N')
				defunflag = true;
				c = getc(file);
				break;
			default: MAYBE_SKIP_LINE
		}
		if (c != '(') {
			while (c_isspace(c = getc(file)));
			if (c != '(') {
				ungetc(c, file);
				continue;
			}
		}

		// Move to the first argument.
		while (c_isspace(c = getc(file)));

		if (type != SYMBOL) {
			// Lisp variable or function name.
			if (c != '"') {
				ungetc(c, file);
				continue;
			}
			c = read_c_string_or_comment(file, -1, false, 0);
		}

		if (generate_globals) {
			ptrdiff_t i = 0;
			char const *svalue = 0;

			if (type != SYMBOL)
				// Skip "," and whitespace.
				do c = getc(file); while (c == ',' || c_isspace(c));

			// Read in the identifier.
			do {
				if (c == EOF) goto eof;
				input_buffer[i++] = c;
				if (i >= sizeof input_buffer)
					fatal("identifier too long");
				c = getc(file);
			} while (!(c == ',' || c_isspace(c)));

			input_buffer[i] = '\0';
			memcpy(name, input_buffer, i + 1);

			if (type == SYMBOL) {
				// Skip "," and whitespace.
				do c = getc(file); while (c == ',' || c_isspace(c));

				if (c != '"') {
					ungetc(c, file);
					continue;
				}
				c = read_c_string_or_comment(file, -1, false, 0);
				svalue = input_buffer;
			}

			if (!defunflag) {
				add_global(type, name, 0, svalue);
				continue;
			}
		}

		if (type == SYMBOL) continue;

		/* DEFVAR_LISP ("name", addr, "doc")
		DEFVAR_LISP ("name", addr /\* doc *\/)
		DEFVAR_LISP ("name", addr, doc: /\* doc *\/)  */

		if (defunflag)
			commas = generate_globals ? 5 : 6;
		else if (defvarperbufferflag)
			commas = 3;
		else // defvarflag
			commas = 1;

		while (commas) {
			if (c == ',') {
				--commas;

				if (defunflag && (commas == 2 || commas == 3)) {
					int scanned = 0;

					while (c_isspace(c = getc(file)));

					if (c == EOF) goto eof;
					ungetc(c, file);
					// Pick up minargs.
					if (commas == 3)
						scanned = fscanf(file, "%d", &minargs);
					// Pick up maxargs.
					else if (c == 'M' || c == 'U') // MANY || UNEVALLED
						maxargs =
							!generate_globals || c == 'M'
							? -1
							: -2;
					else
						scanned = fscanf(file, "%d", &maxargs);
					if (scanned < 0) goto eof;
				}
			}

			if (c == EOF) goto eof;
			c = getc(file);
		}

		if (generate_globals) {
			struct global *const g = add_global(FUNCTION, name, maxargs, 0);
			if (!g) continue;

			// The following code tries to recognize function attributes
			// specified after the docstring, e.g.:
			//
			// DEFUN ("foo", Ffoo, Sfoo, X, Y, Z,
			// doc: /\* doc *\/
			// attributes: attribute1 attribute2 ...)
			// (Lisp_Object arg...)
			//
			// Now only `const' and `noreturn' attributes are used.

			// We are now after ',' that starts a doc arg.

			while (c_isspace(c)) c = getc(file);

			// Allow empty doc arg, e.g. DEFUN("a", Fa, Sa, 0, 0, 0,).
			if (c == ')') continue;

			// Advance to the end of docstring.
			{
				char d = c;
				while (true) {
					c = d;
					d = getc(file);
					if (d == EOF) goto eof;
					if (c == '*' && d == '/') break;
				}
			}
			while (c_isspace(c = getc(file)));
			if (c == EOF) goto eof;

			// Check for 'attributes:' token.
			char const *p = "attributes:";
			while (*p && c == *p) {
				++p;
				c = getc(file);
			}
			if (!*p) {
				char *input_buffer_1 = input_buffer;
				// Collect attributes up to ')'.
				while (true) {
					if (c == EOF) goto eof;
					if (c == ')') break;
					if (input_buffer_1 - input_buffer > sizeof input_buffer) abort();
					*input_buffer_1++ = c;
					c = getc(file);
				}
				*input_buffer_1 = 0;
				if (strstr(input_buffer, "noreturn"))
					g->flags |= DEFUN_noreturn;
				if (strstr(input_buffer, "const"))
					g->flags |= DEFUN_const;
			}
			continue;
		}

		while (c_isspace(c)) c = getc(file);

		if (c == '"') c = read_c_string_or_comment(file, 0, false, 0);

		while (c != EOF && c != ',' && c != '/') c = getc(file);
		if (c == ',') {
			while (c_isspace(c = getc(file)));

			while (c_isalpha(c)) c = getc(file);
			if (c == ':') {
				doc_keyword = true;
				while (c_isspace(c = getc(file)));
			}
		}

		if (
			c == '"'
			|| (
				c == '/'
				&& (
					c = getc(file),
					ungetc(c, file),
					c == '*'
				)
			)
		) {
			bool comment = c != '"';
			bool saw_usage;

			printf("\037%c%s\n", defvarflag ? 'V' : 'F', input_buffer);

			if (comment) getc(file); // Skip past `*'.
			c = read_c_string_or_comment(file, 1, comment, &saw_usage);

			/* If this is a defun, find the arguments and print them.  If
			this function takes MANY or UNEVALLED args, then the C source
			won't give the names of the arguments, so we shouldn't bother
			trying to find them.

			Various doc-string styles:
			0: DEFUN (..., "DOC") (args)            [!comment]
			1: DEFUN (..., /\* DOC *\/ (args))      [comment && !doc_keyword]
			2: DEFUN (..., doc: /\* DOC *\/) (args) [comment && doc_keyword]
			*/
			if (defunflag && maxargs != -1 && !saw_usage) {
				char argbuf[1024], *p = argbuf;

				if (!comment || doc_keyword)
					while (c != ')') {
						if (c == EOF) goto eof;
						c = getc(file);
					}

				// Skip into arguments.
				while (c != '(') {
					if (c == EOF) goto eof;
					c = getc(file);
				}
				// Copy arguments into ARGBUF.
				*p++ = c;
				do {
					c = getc(file);
					if (c == EOF) goto eof;
					*p++ = c;
				} while (c != ')');

				*p = '\0';

				// Output them.

				fputs("\n\n", stdout);

				// Write to stdout the argument names of function input_buffer,
				// whose text is in argbuf.

				bool in_ident = false;
				char *ident_start UNINIT;
				ptrdiff_t ident_length = 0;

				fputs("(fn", stdout);

				for (p = *argbuf == '(' ? argbuf + 1 : argbuf; *p; ++p) {
					c = *p;

					// Notice when a new identifier starts.
					if ((c_isalnum (c) || c == '_') != in_ident) {
						if (!in_ident) {
							in_ident = true;
							ident_start = p;
						} else {
							in_ident = false;
							ident_length = p - ident_start;
						}
					}

					// Found the end of an argument, write out the last seen identifier.
					if (c == ',' || c == ')') {
						if (ident_length == 0) {
							error(
								"empty arg list for '%s' should be (void), not ()",
								input_buffer
							);
							continue;
						}

						if (strncmp(ident_start, "void", ident_length) == 0) continue;

						putchar(' ');

						if (minargs == 0 && maxargs > 0) fputs("&optional ", stdout);

						--minargs;
						--maxargs;

						// In C code, `default' is a reserved word, so we spell it
						// `defalt'; demangle that here.
						if (ident_length == 6 && memcmp(ident_start, "defalt", 6) == 0)
							fputs("DEFAULT", stdout);
						else
							while (ident_length-- > 0) {
								c = c_toupper(*ident_start++);
								// Print underscore as hyphen.
								if (c == '_') c = '-';
								putchar(c);
							}
					}
				}

				putchar(')');
			} else if (defunflag && maxargs == -1 && !saw_usage)
				// The DOC should provide the usage form.
				fprintf(stderr, "Missing 'usage' for function '%s'.\n", input_buffer);
		}
    }
	eof:
	if (ferror(file) || fclose(file) != 0)
		fatal("read error");
}
