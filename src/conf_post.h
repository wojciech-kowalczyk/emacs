/* conf_post.h --- configure.ac includes this via AH_BOTTOM

Copyright (C) 1988, 1993-1994, 1999-2002, 2004-2024 Free Software
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

/* Put the code here rather than in configure.ac using AH_BOTTOM.
   This way, the code does not get processed by autoheader.  For
   example, undefs here are not commented out.  */

/* Disable 'assert' unless enabling checking.  Do this early, in
   case some misguided implementation depends on NDEBUG in some
   include file other than assert.h.  */
#if !defined ENABLE_CHECKING && !defined NDEBUG
# define NDEBUG
#endif

/* To help make dependencies clearer elsewhere, this file typically
   does not #include other files.  The exception is ms-w32.h (WINDOWSNT
   only) because it historically was included here and changing that
   would take some work.  */

#if defined WINDOWSNT && !defined DEFER_MS_W32_H
# include <ms-w32.h>
#endif

/* GNUC_PREREQ (V, W, X) is true if this is GNU C version V.W.X or later.
   It can be used in a preprocessor expression.  */
#ifndef __GNUC_MINOR__
# define GNUC_PREREQ(v, w, x) false
#elif ! defined __GNUC_PATCHLEVEL__
# define GNUC_PREREQ(v, w, x) \
    ((v) < __GNUC__ + ((w) < __GNUC_MINOR__ + ((x) == 0))
#else
# define GNUC_PREREQ(v, w, x) \
    ((v) < __GNUC__ + ((w) < __GNUC_MINOR__ + ((x) <= __GNUC_PATCHLEVEL__)))
#endif

/* The type of bool bitfields.  Needed to compile Objective-C with
   standard GCC, and to make sure adjacent bool_bf fields are packed
   into the same 1-, 2-, or 4-byte allocation unit in the MinGW
   builds.  It was also needed to port to pre-C99 compilers, although
   we don't care about that any more.  */
#if NS_IMPL_GNUSTEP || defined __MINGW32__
typedef unsigned int bool_bf;
#else
typedef bool bool_bf;
#endif

/* A substitute for __has_attribute on compilers that lack it.
   It is used only on arguments like cleanup that are handled here.
   This macro should be used only in #if expressions, as Oracle
   Studio 12.5's __has_attribute does not work in plain code.  */
#if (defined __has_attribute \
     && (!defined __clang_minor__ \
         || 3 < __clang_major__ + (5 <= __clang_minor__)))
# define HAS_ATTRIBUTE(a) __has_attribute (__##a##__)
#else
# define HAS_ATTRIBUTE(a) HAS_ATTR_##a
# define HAS_ATTR_cleanup GNUC_PREREQ (3, 4, 0)
# define HAS_ATTR_no_address_safety_analysis false
# define HAS_ATTR_no_sanitize false
# define HAS_ATTR_no_sanitize_address GNUC_PREREQ (4, 8, 0)
# define HAS_ATTR_no_sanitize_undefined GNUC_PREREQ (4, 9, 0)
#endif

/* A substitute for __has_feature on compilers that lack it.  It is used only
   to define ADDRESS_SANITIZER below.  */
#ifdef __has_feature
# define HAS_FEATURE(a) __has_feature (a)
#else
# define HAS_FEATURE(a) false
#endif

/* True if addresses are being sanitized.  */
#if defined __SANITIZE_ADDRESS__ || HAS_FEATURE (address_sanitizer)
# define ADDRESS_SANITIZER true
#else
# define ADDRESS_SANITIZER false
#endif

#ifdef emacs
/* We include stdlib.h here, because Gnulib's stdlib.h might redirect
   'free' to its replacement, and we want to avoid that in unexec
   builds.  Including it here will render its inclusion after config.h
   a no-op.  */
# if defined DARWIN_OS && defined HAVE_UNEXEC
#  include <stdlib.h>
# endif
#endif

#if defined DARWIN_OS && defined emacs && defined HAVE_UNEXEC
# undef malloc
# define malloc unexec_malloc
# undef realloc
# define realloc unexec_realloc
# undef free
# define free unexec_free

extern void *unexec_malloc (size_t);
extern void *unexec_realloc (void *, size_t);
extern void unexec_free (void *);

#endif

/* We have to go this route, rather than the old hpux9 approach of
   renaming the functions via macros.  The system's stdlib.h has fully
   prototyped declarations, which yields a conflicting definition of
   srand48; it tries to redeclare what was once srandom to be srand48.
   So we go with HAVE_LRAND48 being defined.  */
#ifdef HPUX
#undef srandom
#undef random
#undef HAVE_RANDOM
#undef HAVE_RINT
#endif  /* HPUX */

/* macOS / GNUstep need a bit more pure memory.  Of the existing knobs,
   SYSTEM_PURESIZE_EXTRA seems like the least likely to cause problems.  */
#ifdef HAVE_NS
#if defined NS_IMPL_GNUSTEP
#  define SYSTEM_PURESIZE_EXTRA 30000
#elif defined DARWIN_OS
#  define SYSTEM_PURESIZE_EXTRA 200000
#endif
#endif

#ifdef CYGWIN
#define SYSTEM_PURESIZE_EXTRA 50000
#endif

#if defined HAVE_NTGUI && !defined DebPrint
# ifdef EMACSDEBUG
extern void _DebPrint (const char *fmt, ...);
#  define DebPrint(stuff) _DebPrint stuff
# else
#  define DebPrint(stuff) ((void) 0)
# endif
#endif

#if defined CYGWIN && defined HAVE_NTGUI
# define NTGUI_UNICODE /* Cygwin runs only on UNICODE-supporting systems */
# define _WIN32_WINNT 0x500 /* Win2k */
/* The following was in /usr/include/string.h prior to Cygwin 1.7.33.  */
#ifndef strnicmp
#define strnicmp strncasecmp
#endif
#endif

#ifdef emacs /* Don't do this for lib-src.  */
/* Tell regex.c to use a type compatible with Emacs.  */
#define RE_TRANSLATE_TYPE Lisp_Object
#define RE_TRANSLATE(TBL, C) char_table_translate (TBL, C)
#define RE_TRANSLATE_P(TBL) (!BASE_EQ (TBL, make_fixnum (0)))
#endif

/* Tell time_rz.c to use Emacs's getter and setter for TZ.
   Only Emacs uses time_rz so this is OK.  */
#define getenv_TZ emacs_getenv_TZ
#define setenv_TZ emacs_setenv_TZ
extern char *emacs_getenv_TZ (void);
extern int emacs_setenv_TZ (char const *);

#define NO_INLINE _GL_ATTRIBUTE_NOINLINE
#define EXTERNALLY_VISIBLE _GL_ATTRIBUTE_EXTERNALLY_VISIBLE

#if GNUC_PREREQ (4, 4, 0) && defined __GLIBC_MINOR__
# define PRINTF_ARCHETYPE __gnu_printf__
#elif GNUC_PREREQ (4, 4, 0) && defined __MINGW32__
# ifdef MINGW_W64
/* When __USE_MINGW_ANSI_STDIO is non-zero (as set by config.h),
   MinGW64 replaces printf* with its own versions that are
   __gnu_printf__ compatible, and emits warnings for MS native %I64d
   format spec.  */
#  if __USE_MINGW_ANSI_STDIO
#   define PRINTF_ARCHETYPE __gnu_printf__
#  else
#   define PRINTF_ARCHETYPE __ms_printf__
#  endif
# else	/* mingw.org's MinGW */
/* Starting from runtime v5.0.0, mingw.org's MinGW with GCC 6 and
   later turns on __USE_MINGW_ANSI_STDIO by default, replaces printf*
   with its own __mingw_printf__ version, which still recognizes
   %I64d.  */
#  if GNUC_PREREQ (6, 0, 0) && __MINGW32_MAJOR_VERSION >= 5
#   define PRINTF_ARCHETYPE __mingw_printf__
#  else  /* __MINGW32_MAJOR_VERSION < 5 */
#   define PRINTF_ARCHETYPE __ms_printf__
#  endif  /* __MINGW32_MAJOR_VERSION < 5 */
# endif	 /* MinGW */
#else
# define PRINTF_ARCHETYPE __printf__
#endif
#define ATTRIBUTE_FORMAT_PRINTF(string_index, first_to_check) \
  _GL_ATTRIBUTE_FORMAT ((PRINTF_ARCHETYPE, string_index, first_to_check))

#define ARG_NONNULL _GL_ATTRIBUTE_NONNULL

/* Declare NAME to be a pointer to an object of type TYPE, initialized
   to the address ADDR, which may be of a different type.  Accesses
   via NAME may alias with other accesses with the traditional
   behavior, even if options like gcc -fstrict-aliasing are used.  */

#define DECLARE_POINTER_ALIAS(name, type, addr) \
  type _GL_ATTRIBUTE_MAY_ALIAS *name = (type *) (addr)

#if 3 <= __GNUC__
# define ATTRIBUTE_SECTION(name) __attribute__ ((section (name)))
#else
# define ATTRIBUTE_SECTION(name)
#endif

#define ATTRIBUTE_MALLOC_SIZE(args) \
  _GL_ATTRIBUTE_MALLOC _GL_ATTRIBUTE_ALLOC_SIZE (args)

/* Work around GCC bug 59600: when a function is inlined, the inlined
   code may have its addresses sanitized even if the function has the
   no_sanitize_address attribute.  This bug is fixed in GCC 4.9.0 and
   clang 3.4.  */
#if (! ADDRESS_SANITIZER \
     || (GNUC_PREREQ (4, 9, 0) \
	 || 3 < __clang_major__ + (4 <= __clang_minor__)))
# define ADDRESS_SANITIZER_WORKAROUND /* No workaround needed.  */
#else
# define ADDRESS_SANITIZER_WORKAROUND NO_INLINE
#endif

/* Attribute of functions whose code should not have addresses
   sanitized.  */

#if HAS_ATTRIBUTE (no_sanitize_address)
# define ATTRIBUTE_NO_SANITIZE_ADDRESS \
    __attribute__ ((no_sanitize_address)) ADDRESS_SANITIZER_WORKAROUND
#elif HAS_ATTRIBUTE (no_address_safety_analysis)
# define ATTRIBUTE_NO_SANITIZE_ADDRESS \
    __attribute__ ((no_address_safety_analysis)) ADDRESS_SANITIZER_WORKAROUND
#else
# define ATTRIBUTE_NO_SANITIZE_ADDRESS
#endif

/* Attribute of functions whose undefined behavior should not be sanitized.  */

#if HAS_ATTRIBUTE (no_sanitize_undefined)
# define ATTRIBUTE_NO_SANITIZE_UNDEFINED __attribute__ ((no_sanitize_undefined))
#elif HAS_ATTRIBUTE (no_sanitize)
# define ATTRIBUTE_NO_SANITIZE_UNDEFINED \
    __attribute__ ((no_sanitize ("undefined")))
#else
# define ATTRIBUTE_NO_SANITIZE_UNDEFINED
#endif

/* gcc -fsanitize=address does not work with vfork in Fedora 28 x86-64.  See:
   https://lists.gnu.org/r/emacs-devel/2017-05/msg00464.html
   For now, assume that this problem occurs on all platforms.  */
#if ADDRESS_SANITIZER && !defined vfork
# define vfork fork
#endif

/* vfork is deprecated on at least macOS 11.6 and later, but it still works
   and is faster than fork, so silence the warning as if we knew what we
   are doing.  */
#ifdef DARWIN_OS
#define VFORK()								\
  (_Pragma("clang diagnostic push")					\
   _Pragma("clang diagnostic ignored \"-Wdeprecated-declarations\"")	\
   vfork ()								\
   _Pragma("clang diagnostic pop"))
#else
#define VFORK() vfork ()
#endif

#if ! (defined __FreeBSD__ || defined GNU_LINUX || defined __MINGW32__)
# undef PROFILING
#endif

/* Some versions of GNU/Linux define noinline in their headers.  */
#ifdef noinline
#undef noinline
#endif

/* INLINE marks functions defined in Emacs-internal C headers.
   INLINE is implemented via C99-style 'extern inline' if Emacs is built
   with -DEMACS_EXTERN_INLINE; otherwise it is implemented via 'static'.
   EMACS_EXTERN_INLINE is no longer the default, as 'static' seems to
   have better performance with GCC.

   An include file foo.h should prepend INLINE to function
   definitions, with the following overall pattern:

      [#include any other .h files first.]
      ...
      INLINE_HEADER_BEGIN
      ...
      INLINE int
      incr (int i)
      {
        return i + 1;
      }
      ...
      INLINE_HEADER_END

   For every executable, exactly one file that includes the header
   should do this:

      #define INLINE EXTERN_INLINE

   before including config.h or any other .h file.
   Other .c files should not define INLINE.
   For Emacs, this is done by having emacs.c first '#define INLINE
   EXTERN_INLINE' and then include every .h file that uses INLINE.

   The INLINE_HEADER_BEGIN and INLINE_HEADER_END macros suppress bogus
   warnings in some GCC versions; see ../m4/extern-inline.m4.  */

#ifdef EMACS_EXTERN_INLINE

/* Use Gnulib's extern-inline module for extern inline functions.

   C99 compilers compile functions like 'incr' as C99-style extern
   inline functions.  Buggy GCC implementations do something similar with
   GNU-specific keywords.  Buggy non-GCC compilers use static
   functions, which bloats the code but is good enough.  */

# ifndef INLINE
#  define INLINE _GL_INLINE
# endif
# define EXTERN_INLINE _GL_EXTERN_INLINE
# define INLINE_HEADER_BEGIN _GL_INLINE_HEADER_BEGIN
# define INLINE_HEADER_END _GL_INLINE_HEADER_END

#else

/* Use 'static inline' instead of 'extern inline' because 'static inline'
   has much better performance for Emacs when compiled with 'gcc -Og'.  */

# ifndef INLINE
#  define INLINE EXTERN_INLINE
# endif
# define EXTERN_INLINE static inline
# define INLINE_HEADER_BEGIN
# define INLINE_HEADER_END

#endif

/* 'int x UNINIT;' is equivalent to 'int x;', except it cajoles GCC
   into not warning incorrectly about use of an uninitialized variable.  */
#if defined GCC_LINT || defined lint
# define UNINIT = {0,}
#else
# define UNINIT /* empty */
#endif

/* MB_CUR_MAX is often broken on systems which copy-paste LLVM
   headers, so replace its definition with a working one if
   necessary.  */

#ifdef REPLACEMENT_MB_CUR_MAX
#include <stdlib.h>
#undef MB_CUR_MAX
#define MB_CUR_MAX REPLACEMENT_MB_CUR_MAX
#endif /* REPLACEMENT_MB_CUR_MAX */

/* Emacs does not need glibc strftime behavior for AM and PM
   indicators.  */
#define REQUIRE_GNUISH_STRFTIME_AM_PM false
