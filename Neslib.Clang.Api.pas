unit Neslib.Clang.Api;
{< LibClang 9.0.0 Header Translation }

{$IFNDEF MSWINDOWS}
  {$MESSAGE Error 'Clang for Delphi (currently) only works on Windows'}
{$ENDIF}

{$MINENUMSIZE 4}

interface

const
  LIBCLANG = 'libclang.dll';

type
  time_t = LongInt;
  SIZE_T = NativeUInt;
  PSIZE_T = ^SIZE_T;

{$REGION 'CXErrorCode.h'}
(*===-- clang-c/CXErrorCode.h - C Index Error Codes  --------------*- C -*-===*\
|*                                                                            *|
|* Part of the LLVM Project, under the Apache License v2.0 with LLVM          *|
|* Exceptions.                                                                *|
|* See https://llvm.org/LICENSE.txt for license information.                  *|
|* SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception                    *|
|*                                                                            *|
|*===----------------------------------------------------------------------===*|
|*                                                                            *|
|* This header provides the CXErrorCode enumerators.                          *|
|*                                                                            *|
\*===----------------------------------------------------------------------===*)

(**
 * Error codes returned by libclang routines.
 *
 * Zero (\c CXError_Success) is the only error code indicating success.  Other
 * error codes, including not yet assigned non-zero values, indicate errors.
 *)
type
  TCXErrorCode = Integer;
  PCXErrorCode = ^TCXErrorCode;

const
  CXError_Success = 0;
  CXError_Failure = 1;
  CXError_Crashed = 2;
  CXError_InvalidArguments = 3;
  CXError_ASTReadError = 4;
{$ENDREGION 'CXErrorCode.h'}

{$REGION 'CXString.h'}
(*===-- clang-c/CXString.h - C Index strings  --------------------*- C -*-===*\
|*                                                                            *|
|* Part of the LLVM Project, under the Apache License v2.0 with LLVM          *|
|* Exceptions.                                                                *|
|* See https://llvm.org/LICENSE.txt for license information.                  *|
|* SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception                    *|
|*                                                                            *|
|*===----------------------------------------------------------------------===*|
|*                                                                            *|
|* This header provides the interface to C Index strings.                     *|
|*                                                                            *|
\*===----------------------------------------------------------------------===*)

(**
 * \defgroup CINDEX_STRING String manipulation routines
 * \ingroup CINDEX
 *
 * @{
 *)

(**
 * A character string.
 *
 * The \c CXString type is used to return strings from the interface when
 * the ownership of that string might differ from one call to the next.
 * Use \c clang_getCString() to retrieve the string data and, once finished
 * with the string data, call \c clang_disposeString() to free the string.
 *)
type
  {$IFDEF WIN32}
  { Some C APIs return a TCXString record.
    However, when a returned value fits into 64-Bits on Win32, then Delphi
    doesn't return a TCXString record correctly. So we use an UInt64 instead. }
  TCXString = UInt64;
  {$ELSE}
  TCXString = record
    data: Pointer;
    private_flags: Cardinal;
  end;
  {$ENDIF}
  PCXString = ^TCXString;

type
  TCXStringSet = record
    Strings: PCXString;
    Count: Cardinal;
  end;
  PCXStringSet = ^TCXStringSet;

(**
 * Retrieve the character data associated with the given string.
 *)
function clang_getCString(_string: TCXString): PAnsiChar; cdecl external LIBCLANG;

(**
 * Free the given string.
 *)
procedure clang_disposeString(_string: TCXString); cdecl external LIBCLANG;

(**
 * Free the given string set.
 *)
procedure clang_disposeStringSet(_set: PCXStringSet); cdecl external LIBCLANG;
{$ENDREGION 'CXString.h'}

{$REGION 'CXCompilationDatabase.h'}
(*===-- clang-c/CXCompilationDatabase.h - Compilation database  ---*- C -*-===*\
|*                                                                            *|
|* Part of the LLVM Project, under the Apache License v2.0 with LLVM          *|
|* Exceptions.                                                                *|
|* See https://llvm.org/LICENSE.txt for license information.                  *|
|* SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception                    *|
|*                                                                            *|
|*===----------------------------------------------------------------------===*|
|*                                                                            *|
|* This header provides a public interface to use CompilationDatabase without *|
|* the full Clang C++ API.                                                    *|
|*                                                                            *|
\*===----------------------------------------------------------------------===*)

(** \defgroup COMPILATIONDB CompilationDatabase functions
 * \ingroup CINDEX
 *
 * @{
 *)

(**
 * A compilation database holds all information used to compile files in a
 * project. For each file in the database, it can be queried for the working
 * directory or the command line used for the compiler invocation.
 *
 * Must be freed by \c clang_CompilationDatabase_dispose
 *)
type TCXCompilationDatabase = Pointer;

(**
 * Contains the results of a search in the compilation database
 *
 * When searching for the compile command for a file, the compilation db can
 * return several commands, as the file may have been compiled with
 * different options in different places of the project. This choice of compile
 * commands is wrapped in this opaque data structure. It must be freed by
 * \c clang_CompileCommands_dispose.
 *)
type TCXCompileCommands = Pointer;

(**
 * Represents the command line invocation to compile a specific file.
 *)
type TCXCompileCommand = Pointer;

(**
 * Error codes for Compilation Database
 *)
type
  TCXCompilationDatabase_Error = Integer;
  PCXCompilationDatabase_Error = ^TCXCompilationDatabase_Error;

const
  CXCompilationDatabase_NoError = 0;
  CXCompilationDatabase_CanNotLoadDatabase = 1;

(**
 * Creates a compilation database from the database found in directory
 * buildDir. For example, CMake can output a compile_commands.json which can
 * be used to build the database.
 *
 * It must be freed by \c clang_CompilationDatabase_dispose.
 *)
function clang_CompilationDatabase_fromDirectory(const BuildDir: PAnsiChar; ErrorCode: PCXCompilationDatabase_Error): TCXCompilationDatabase; cdecl external LIBCLANG;

(**
 * Free the given compilation database
 *)
procedure clang_CompilationDatabase_dispose(p1: TCXCompilationDatabase); cdecl external LIBCLANG;

(**
 * Find the compile commands used for a file. The compile commands
 * must be freed by \c clang_CompileCommands_dispose.
 *)
function clang_CompilationDatabase_getCompileCommands(p1: TCXCompilationDatabase; const CompleteFileName: PAnsiChar): TCXCompileCommands; cdecl external LIBCLANG;

(**
 * Get all the compile commands in the given compilation database.
 *)
function clang_CompilationDatabase_getAllCompileCommands(p1: TCXCompilationDatabase): TCXCompileCommands; cdecl external LIBCLANG;

(**
 * Free the given CompileCommands
 *)
procedure clang_CompileCommands_dispose(p1: TCXCompileCommands); cdecl external LIBCLANG;

(**
 * Get the number of CompileCommand we have for a file
 *)
function clang_CompileCommands_getSize(p1: TCXCompileCommands): Cardinal; cdecl external LIBCLANG;

(**
 * Get the I'th CompileCommand for a file
 *
 * Note : 0 <= i < clang_CompileCommands_getSize(CXCompileCommands)
 *)
function clang_CompileCommands_getCommand(p1: TCXCompileCommands; I: Cardinal): TCXCompileCommand; cdecl external LIBCLANG;

(**
 * Get the working directory where the CompileCommand was executed from
 *)
function clang_CompileCommand_getDirectory(p1: TCXCompileCommand): TCXString; cdecl external LIBCLANG;

(**
 * Get the filename associated with the CompileCommand.
 *)
function clang_CompileCommand_getFilename(p1: TCXCompileCommand): TCXString; cdecl external LIBCLANG;

(**
 * Get the number of arguments in the compiler invocation.
 *
 *)
function clang_CompileCommand_getNumArgs(p1: TCXCompileCommand): Cardinal; cdecl external LIBCLANG;

(**
 * Get the I'th argument value in the compiler invocations
 *
 * Invariant :
 *  - argument 0 is the compiler executable
 *)
function clang_CompileCommand_getArg(p1: TCXCompileCommand; I: Cardinal): TCXString; cdecl external LIBCLANG;

(**
 * Get the number of source mappings for the compiler invocation.
 *)
function clang_CompileCommand_getNumMappedSources(p1: TCXCompileCommand): Cardinal; cdecl external LIBCLANG;

(**
 * Get the I'th mapped source path for the compiler invocation.
 *)
function clang_CompileCommand_getMappedSourcePath(p1: TCXCompileCommand; I: Cardinal): TCXString; cdecl external LIBCLANG;

(**
 * Get the I'th mapped source content for the compiler invocation.
 *)
function clang_CompileCommand_getMappedSourceContent(p1: TCXCompileCommand; I: Cardinal): TCXString; cdecl external LIBCLANG;
{$ENDREGION 'CXCompilationDatabase.h'}

{$REGION 'BuildSystem.h'}
(*==-- clang-c/BuildSystem.h - Utilities for use by build systems -*- C -*-===*\
|*                                                                            *|
|* Part of the LLVM Project, under the Apache License v2.0 with LLVM          *|
|* Exceptions.                                                                *|
|* See https://llvm.org/LICENSE.txt for license information.                  *|
|* SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception                    *|
|*                                                                            *|
|*===----------------------------------------------------------------------===*|
|*                                                                            *|
|* This header provides various utilities for use by build systems.           *|
|*                                                                            *|
\*===----------------------------------------------------------------------===*)

(**
 * \defgroup BUILD_SYSTEM Build system utilities
 * @{
 *)

(**
 * Return the timestamp for use with Clang's
 * \c -fbuild-session-timestamp= option.
 *)
function clang_getBuildSessionTimestamp(): UInt64; cdecl external LIBCLANG;

(**
 * Object encapsulating information about overlaying virtual
 * file/directories over the real file system.
 *)
type TCXVirtualFileOverlay = Pointer;

(**
 * Create a \c CXVirtualFileOverlay object.
 * Must be disposed with \c clang_VirtualFileOverlay_dispose().
 *
 * \param options is reserved, always pass 0.
 *)
function clang_VirtualFileOverlay_create(options: Cardinal): TCXVirtualFileOverlay; cdecl external LIBCLANG;

(**
 * Map an absolute virtual file path to an absolute real one.
 * The virtual path must be canonicalized (not contain "."/"..").
 * \returns 0 for success, non-zero to indicate an error.
 *)
function clang_VirtualFileOverlay_addFileMapping(p1: TCXVirtualFileOverlay; const virtualPath: PAnsiChar; const realPath: PAnsiChar): TCXErrorCode; cdecl external LIBCLANG;

(**
 * Set the case sensitivity for the \c CXVirtualFileOverlay object.
 * The \c CXVirtualFileOverlay object is case-sensitive by default, this
 * option can be used to override the default.
 * \returns 0 for success, non-zero to indicate an error.
 *)
function clang_VirtualFileOverlay_setCaseSensitivity(p1: TCXVirtualFileOverlay; caseSensitive: Integer): TCXErrorCode; cdecl external LIBCLANG;

(**
 * Write out the \c CXVirtualFileOverlay object to a char buffer.
 *
 * \param options is reserved, always pass 0.
 * \param out_buffer_ptr pointer to receive the buffer pointer, which should be
 * disposed using \c clang_free().
 * \param out_buffer_size pointer to receive the buffer size.
 * \returns 0 for success, non-zero to indicate an error.
 *)
function clang_VirtualFileOverlay_writeToBuffer(p1: TCXVirtualFileOverlay; options: Cardinal; out_buffer_ptr: PPAnsiChar; out_buffer_size: PCardinal): TCXErrorCode; cdecl external LIBCLANG;

(**
 * free memory allocated by libclang, such as the buffer returned by
 * \c CXVirtualFileOverlay() or \c clang_ModuleMapDescriptor_writeToBuffer().
 *
 * \param buffer memory pointer to free.
 *)
procedure clang_free(buffer: Pointer); cdecl external LIBCLANG;

(**
 * Dispose a \c CXVirtualFileOverlay object.
 *)
procedure clang_VirtualFileOverlay_dispose(p1: TCXVirtualFileOverlay); cdecl external LIBCLANG;

(**
 * Object encapsulating information about a module.map file.
 *)
type TCXModuleMapDescriptor = Pointer;

(**
 * Create a \c CXModuleMapDescriptor object.
 * Must be disposed with \c clang_ModuleMapDescriptor_dispose().
 *
 * \param options is reserved, always pass 0.
 *)
function clang_ModuleMapDescriptor_create(options: Cardinal): TCXModuleMapDescriptor; cdecl external LIBCLANG;

(**
 * Sets the framework module name that the module.map describes.
 * \returns 0 for success, non-zero to indicate an error.
 *)
function clang_ModuleMapDescriptor_setFrameworkModuleName(p1: TCXModuleMapDescriptor; const name: PAnsiChar): TCXErrorCode; cdecl external LIBCLANG;

(**
 * Sets the umbrealla header name that the module.map describes.
 * \returns 0 for success, non-zero to indicate an error.
 *)
function clang_ModuleMapDescriptor_setUmbrellaHeader(p1: TCXModuleMapDescriptor; const name: PAnsiChar): TCXErrorCode; cdecl external LIBCLANG;

(**
 * Write out the \c CXModuleMapDescriptor object to a char buffer.
 *
 * \param options is reserved, always pass 0.
 * \param out_buffer_ptr pointer to receive the buffer pointer, which should be
 * disposed using \c clang_free().
 * \param out_buffer_size pointer to receive the buffer size.
 * \returns 0 for success, non-zero to indicate an error.
 *)
function clang_ModuleMapDescriptor_writeToBuffer(p1: TCXModuleMapDescriptor; options: Cardinal; out_buffer_ptr: PPAnsiChar; out_buffer_size: PCardinal): TCXErrorCode; cdecl external LIBCLANG;

(**
 * Dispose a \c CXModuleMapDescriptor object.
 *)
procedure clang_ModuleMapDescriptor_dispose(p1: TCXModuleMapDescriptor); cdecl external LIBCLANG;
{$ENDREGION 'BuildSystem.h'}

{$REGION 'Index.h'}
(*===-- clang-c/Index.h - Indexing Public C Interface -------------*- C -*-===*\
|*                                                                            *|
|* Part of the LLVM Project, under the Apache License v2.0 with LLVM          *|
|* Exceptions.                                                                *|
|* See https://llvm.org/LICENSE.txt for license information.                  *|
|* SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception                    *|
|*                                                                            *|
|*===----------------------------------------------------------------------===*|
|*                                                                            *|
|* This header provides a public interface to a Clang library for extracting  *|
|* high-level symbol information from source files without exposing the full  *|
|* Clang C++ API.                                                             *|
|*                                                                            *|
\*===----------------------------------------------------------------------===*)

(**
 * The version constants for the libclang API.
 * CINDEX_VERSION_MINOR should increase when there are API additions.
 * CINDEX_VERSION_MAJOR is intended for "major" source/ABI breaking changes.
 *
 * The policy about the libclang API was always to keep it source and ABI
 * compatible, thus CINDEX_VERSION_MAJOR is expected to remain stable.
 *)
const CINDEX_VERSION_MAJOR = 0;
const CINDEX_VERSION_MINOR = 59;

{!#define CINDEX_VERSION_ENCODE(major, minor) (
      ((major) * 10000)
    + ((minor) *     1))}

const CINDEX_VERSION = (CINDEX_VERSION_MAJOR * 10000) + CINDEX_VERSION_MINOR;

{!#define CINDEX_VERSION_STRINGIZE_(major, minor)
    #major"."#minor}
{!#define CINDEX_VERSION_STRINGIZE(major, minor)
    CINDEX_VERSION_STRINGIZE_(major, minor)}

const CINDEX_VERSION_STRING = '0.59';

(** \defgroup CINDEX libclang: C Interface to Clang
 *
 * The C Interface to Clang provides a relatively small API that exposes
 * facilities for parsing source code into an abstract syntax tree (AST),
 * loading already-parsed ASTs, traversing the AST, associating
 * physical source locations with elements within the AST, and other
 * facilities that support Clang-based development tools.
 *
 * This C interface to Clang will never provide all of the information
 * representation stored in Clang's C++ AST, nor should it: the intent is to
 * maintain an API that is relatively stable from one release to the next,
 * providing only the basic functionality needed to support development tools.
 *
 * To avoid namespace pollution, data types are prefixed with "CX" and
 * functions are prefixed with "clang_".
 *
 * @{
 *)

(**
 * An "index" that consists of a set of translation units that would
 * typically be linked together into an executable or library.
 *)
type TCXIndex = Pointer;

(**
 * An opaque type representing target information for a given translation
 * unit.
 *)
type TCXTargetInfo = Pointer;

(**
 * A single translation unit, which resides in an index.
 *)
type TCXTranslationUnit = Pointer;
type PCXTranslationUnit = ^TCXTranslationUnit;

(**
 * Opaque pointer representing client data that will be passed through
 * to various callbacks and visitors.
 *)
type TCXClientData = Pointer;

(**
 * Provides the contents of a file that has not yet been saved to disk.
 *
 * Each CXUnsavedFile instance provides the name of a file on the
 * system along with the current contents of that file that have not
 * yet been saved to disk.
 *)
type
  TCXUnsavedFile = record
    Filename: PAnsiChar;
    Contents: PAnsiChar;
    Length: Longword;
  end;
  PCXUnsavedFile = ^TCXUnsavedFile;

(**
 * Describes the availability of a particular entity, which indicates
 * whether the use of this entity will result in a warning or error due to
 * it being deprecated or unavailable.
 *)
type
  TCXAvailabilityKind = Integer;

const
  CXAvailability_Available = 0;
  CXAvailability_Deprecated = CXAvailability_Available + 1;
  CXAvailability_NotAvailable = CXAvailability_Deprecated + 1;
  CXAvailability_NotAccessible = CXAvailability_NotAvailable + 1;

(**
 * Describes a version number of the form major.minor.subminor.
 *)
type
  TCXVersion = record
    Major: Integer;
    Minor: Integer;
    Subminor: Integer;
  end;
  PCXVersion = ^TCXVersion;

(**
 * Describes the exception specification of a cursor.
 *
 * A negative value indicates that the cursor is not a function declaration.
 *)
type
  TCXCursor_ExceptionSpecificationKind = Integer;

const
  CXCursor_ExceptionSpecificationKind_None = 0;
  CXCursor_ExceptionSpecificationKind_DynamicNone = CXCursor_ExceptionSpecificationKind_None + 1;
  CXCursor_ExceptionSpecificationKind_Dynamic = CXCursor_ExceptionSpecificationKind_DynamicNone + 1;
  CXCursor_ExceptionSpecificationKind_MSAny = CXCursor_ExceptionSpecificationKind_Dynamic + 1;
  CXCursor_ExceptionSpecificationKind_BasicNoexcept = CXCursor_ExceptionSpecificationKind_MSAny + 1;
  CXCursor_ExceptionSpecificationKind_ComputedNoexcept = CXCursor_ExceptionSpecificationKind_BasicNoexcept + 1;
  CXCursor_ExceptionSpecificationKind_Unevaluated = CXCursor_ExceptionSpecificationKind_ComputedNoexcept + 1;
  CXCursor_ExceptionSpecificationKind_Uninstantiated = CXCursor_ExceptionSpecificationKind_Unevaluated + 1;
  CXCursor_ExceptionSpecificationKind_Unparsed = CXCursor_ExceptionSpecificationKind_Uninstantiated + 1;
  CXCursor_ExceptionSpecificationKind_NoThrow = CXCursor_ExceptionSpecificationKind_Unparsed + 1;

(**
 * Provides a shared context for creating translation units.
 *
 * It provides two options:
 *
 * - excludeDeclarationsFromPCH: When non-zero, allows enumeration of "local"
 * declarations (when loading any new translation units). A "local" declaration
 * is one that belongs in the translation unit itself and not in a precompiled
 * header that was used by the translation unit. If zero, all declarations
 * will be enumerated.
 *
 * Here is an example:
 *
 * \code
 *   // excludeDeclsFromPCH = 1, displayDiagnostics=1
 *   Idx = clang_createIndex(1, 1);
 *
 *   // IndexTest.pch was produced with the following command:
 *   // "clang -x c IndexTest.h -emit-ast -o IndexTest.pch"
 *   TU = clang_createTranslationUnit(Idx, "IndexTest.pch");
 *
 *   // This will load all the symbols from 'IndexTest.pch'
 *   clang_visitChildren(clang_getTranslationUnitCursor(TU),
 *                       TranslationUnitVisitor, 0);
 *   clang_disposeTranslationUnit(TU);
 *
 *   // This will load all the symbols from 'IndexTest.c', excluding symbols
 *   // from 'IndexTest.pch'.
 *   char *args[] = { "-Xclang", "-include-pch=IndexTest.pch" };
 *   TU = clang_createTranslationUnitFromSourceFile(Idx, "IndexTest.c", 2, args,
 *                                                  0, 0);
 *   clang_visitChildren(clang_getTranslationUnitCursor(TU),
 *                       TranslationUnitVisitor, 0);
 *   clang_disposeTranslationUnit(TU);
 * \endcode
 *
 * This process of creating the 'pch', loading it separately, and using it (via
 * -include-pch) allows 'excludeDeclsFromPCH' to remove redundant callbacks
 * (which gives the indexer the same performance benefit as the compiler).
 *)
function clang_createIndex(excludeDeclarationsFromPCH: Integer; displayDiagnostics: Integer): TCXIndex; cdecl external LIBCLANG;

(**
 * Destroy the given index.
 *
 * The index must not be destroyed until all of the translation units created
 * within that index have been destroyed.
 *)
procedure clang_disposeIndex(index: TCXIndex); cdecl external LIBCLANG;

type
  TCXGlobalOptFlags = Integer;

const
  CXGlobalOpt_None = $0;
  CXGlobalOpt_ThreadBackgroundPriorityForIndexing = $1;
  CXGlobalOpt_ThreadBackgroundPriorityForEditing = $2;
  CXGlobalOpt_ThreadBackgroundPriorityForAll = CXGlobalOpt_ThreadBackgroundPriorityForIndexing
    or CXGlobalOpt_ThreadBackgroundPriorityForEditing;

(**
 * Sets general options associated with a CXIndex.
 *
 * For example:
 * \code
 * CXIndex idx = ...;
 * clang_CXIndex_setGlobalOptions(idx,
 *     clang_CXIndex_getGlobalOptions(idx) |
 *     CXGlobalOpt_ThreadBackgroundPriorityForIndexing);
 * \endcode
 *
 * \param options A bitmask of options, a bitwise OR of CXGlobalOpt_XXX flags.
 *)
procedure clang_CXIndex_setGlobalOptions(p1: TCXIndex; options: Cardinal); cdecl external LIBCLANG;

(**
 * Gets the general options associated with a CXIndex.
 *
 * \returns A bitmask of options, a bitwise OR of CXGlobalOpt_XXX flags that
 * are associated with the given CXIndex object.
 *)
function clang_CXIndex_getGlobalOptions(p1: TCXIndex): Cardinal; cdecl external LIBCLANG;

(**
 * Sets the invocation emission path option in a CXIndex.
 *
 * The invocation emission path specifies a path which will contain log
 * files for certain libclang invocations. A null value (default) implies that
 * libclang invocations are not logged..
 *)
procedure clang_CXIndex_setInvocationEmissionPathOption(p1: TCXIndex; const Path: PAnsiChar); cdecl external LIBCLANG;

(**
 * \defgroup CINDEX_FILES File manipulation routines
 *
 * @{
 *)

(**
 * A particular source file that is part of a translation unit.
 *)
type TCXFile = Pointer;
type PCXFile = ^TCXFile;

(**
 * Retrieve the complete file and path name of the given file.
 *)
function clang_getFileName(SFile: TCXFile): TCXString; cdecl external LIBCLANG;

(**
 * Retrieve the last modification time of the given file.
 *)
function clang_getFileTime(SFile: TCXFile): time_t; cdecl external LIBCLANG;

(**
 * Uniquely identifies a CXFile, that refers to the same underlying file,
 * across an indexing session.
 *)
type
  TCXFileUniqueID = record
    data: array [0..3-1] of UInt64;
  end;
  PCXFileUniqueID = ^TCXFileUniqueID;

(**
 * Retrieve the unique ID for the given \c file.
 *
 * \param file the file to get the ID for.
 * \param outID stores the returned CXFileUniqueID.
 * \returns If there was a failure getting the unique ID, returns non-zero,
 * otherwise returns 0.
*)
function clang_getFileUniqueID(_file: TCXFile; outID: PCXFileUniqueID): Integer; cdecl external LIBCLANG;

(**
 * Determine whether the given header is guarded against
 * multiple inclusions, either with the conventional
 * \#ifndef/\#define/\#endif macro guards or with \#pragma once.
 *)
function clang_isFileMultipleIncludeGuarded(tu: TCXTranslationUnit; _file: TCXFile): Cardinal; cdecl external LIBCLANG;

(**
 * Retrieve a file handle within the given translation unit.
 *
 * \param tu the translation unit
 *
 * \param file_name the name of the file.
 *
 * \returns the file handle for the named file in the translation unit \p tu,
 * or a NULL file handle if the file was not a part of this translation unit.
 *)
function clang_getFile(tu: TCXTranslationUnit; const file_name: PAnsiChar): TCXFile; cdecl external LIBCLANG;

(**
 * Retrieve the buffer associated with the given file.
 *
 * \param tu the translation unit
 *
 * \param file the file for which to retrieve the buffer.
 *
 * \param size [out] if non-NULL, will be set to the size of the buffer.
 *
 * \returns a pointer to the buffer in memory that holds the contents of
 * \p file, or a NULL pointer when the file is not loaded.
 *)
function clang_getFileContents(tu: TCXTranslationUnit; _file: TCXFile; size: PSIZE_T): PAnsiChar; cdecl external LIBCLANG;

(**
 * Returns non-zero if the \c file1 and \c file2 point to the same file,
 * or they are both NULL.
 *)
function clang_File_isEqual(file1: TCXFile; file2: TCXFile): Integer; cdecl external LIBCLANG;

(**
 * Returns the real path name of \c file.
 *
 * An empty string may be returned. Use \c clang_getFileName() in that case.
 *)
function clang_File_tryGetRealPathName(_file: TCXFile): TCXString; cdecl external LIBCLANG;

(**
 * @}
 *)

(**
 * \defgroup CINDEX_LOCATIONS Physical source locations
 *
 * Clang represents physical source locations in its abstract syntax tree in
 * great detail, with file, line, and column information for the majority of
 * the tokens parsed in the source code. These data types and functions are
 * used to represent source location information, either for a particular
 * point in the program or for a range of points in the program, and extract
 * specific location information from those data types.
 *
 * @{
 *)

(**
 * Identifies a specific source location within a translation
 * unit.
 *
 * Use clang_getExpansionLocation() or clang_getSpellingLocation()
 * to map a source location to a particular file, line, and column.
 *)
type
  TCXSourceLocation = record
    ptr_data: array [0..2-1] of Pointer;
    int_data: Cardinal;
  end;
  PCXSourceLocation = ^TCXSourceLocation;

(**
 * Identifies a half-open character range in the source code.
 *
 * Use clang_getRangeStart() and clang_getRangeEnd() to retrieve the
 * starting and end locations from a source range, respectively.
 *)
type
  TCXSourceRange = record
    ptr_data: array [0..2-1] of Pointer;
    begin_int_data: Cardinal;
    end_int_data: Cardinal;
  end;
  PCXSourceRange = ^TCXSourceRange;

(**
 * Retrieve a NULL (invalid) source location.
 *)
function clang_getNullLocation(): TCXSourceLocation; cdecl external LIBCLANG;

(**
 * Determine whether two source locations, which must refer into
 * the same translation unit, refer to exactly the same point in the source
 * code.
 *
 * \returns non-zero if the source locations refer to the same location, zero
 * if they refer to different locations.
 *)
function clang_equalLocations(loc1: TCXSourceLocation; loc2: TCXSourceLocation): Cardinal; cdecl external LIBCLANG;

(**
 * Retrieves the source location associated with a given file/line/column
 * in a particular translation unit.
 *)
function clang_getLocation(tu: TCXTranslationUnit; _file: TCXFile; line: Cardinal; column: Cardinal): TCXSourceLocation; cdecl external LIBCLANG;
(**
 * Retrieves the source location associated with a given character offset
 * in a particular translation unit.
 *)
function clang_getLocationForOffset(tu: TCXTranslationUnit; _file: TCXFile; offset: Cardinal): TCXSourceLocation; cdecl external LIBCLANG;

(**
 * Returns non-zero if the given source location is in a system header.
 *)
function clang_Location_isInSystemHeader(location: TCXSourceLocation): Integer; cdecl external LIBCLANG;

(**
 * Returns non-zero if the given source location is in the main file of
 * the corresponding translation unit.
 *)
function clang_Location_isFromMainFile(location: TCXSourceLocation): Integer; cdecl external LIBCLANG;

(**
 * Retrieve a NULL (invalid) source range.
 *)
function clang_getNullRange(): TCXSourceRange; cdecl external LIBCLANG;

(**
 * Retrieve a source range given the beginning and ending source
 * locations.
 *)
function clang_getRange(_begin: TCXSourceLocation; _end: TCXSourceLocation): TCXSourceRange; cdecl external LIBCLANG;

(**
 * Determine whether two ranges are equivalent.
 *
 * \returns non-zero if the ranges are the same, zero if they differ.
 *)
function clang_equalRanges(range1: TCXSourceRange; range2: TCXSourceRange): Cardinal; cdecl external LIBCLANG;

(**
 * Returns non-zero if \p range is null.
 *)
function clang_Range_isNull(range: TCXSourceRange): Integer; cdecl external LIBCLANG;

(**
 * Retrieve the file, line, column, and offset represented by
 * the given source location.
 *
 * If the location refers into a macro expansion, retrieves the
 * location of the macro expansion.
 *
 * \param location the location within a source file that will be decomposed
 * into its parts.
 *
 * \param file [out] if non-NULL, will be set to the file to which the given
 * source location points.
 *
 * \param line [out] if non-NULL, will be set to the line to which the given
 * source location points.
 *
 * \param column [out] if non-NULL, will be set to the column to which the given
 * source location points.
 *
 * \param offset [out] if non-NULL, will be set to the offset into the
 * buffer to which the given source location points.
 *)
procedure clang_getExpansionLocation(location: TCXSourceLocation; _file: PCXFile; line: PCardinal; column: PCardinal; offset: PCardinal); cdecl external LIBCLANG;

(**
 * Retrieve the file, line and column represented by the given source
 * location, as specified in a # line directive.
 *
 * Example: given the following source code in a file somefile.c
 *
 * \code
 * #123 "dummy.c" 1
 *
 * static int func(void)
 * {
 *     return 0;
 * }
 * \endcode
 *
 * the location information returned by this function would be
 *
 * File: dummy.c Line: 124 Column: 12
 *
 * whereas clang_getExpansionLocation would have returned
 *
 * File: somefile.c Line: 3 Column: 12
 *
 * \param location the location within a source file that will be decomposed
 * into its parts.
 *
 * \param filename [out] if non-NULL, will be set to the filename of the
 * source location. Note that filenames returned will be for "virtual" files,
 * which don't necessarily exist on the machine running clang - e.g. when
 * parsing preprocessed output obtained from a different environment. If
 * a non-NULL value is passed in, remember to dispose of the returned value
 * using \c clang_disposeString() once you've finished with it. For an invalid
 * source location, an empty string is returned.
 *
 * \param line [out] if non-NULL, will be set to the line number of the
 * source location. For an invalid source location, zero is returned.
 *
 * \param column [out] if non-NULL, will be set to the column number of the
 * source location. For an invalid source location, zero is returned.
 *)
procedure clang_getPresumedLocation(location: TCXSourceLocation; filename: PCXString; line: PCardinal; column: PCardinal); cdecl external LIBCLANG;

(**
 * Legacy API to retrieve the file, line, column, and offset represented
 * by the given source location.
 *
 * This interface has been replaced by the newer interface
 * #clang_getExpansionLocation(). See that interface's documentation for
 * details.
 *)
procedure clang_getInstantiationLocation(location: TCXSourceLocation; _file: PCXFile; line: PCardinal; column: PCardinal; offset: PCardinal); cdecl external LIBCLANG;

(**
 * Retrieve the file, line, column, and offset represented by
 * the given source location.
 *
 * If the location refers into a macro instantiation, return where the
 * location was originally spelled in the source file.
 *
 * \param location the location within a source file that will be decomposed
 * into its parts.
 *
 * \param file [out] if non-NULL, will be set to the file to which the given
 * source location points.
 *
 * \param line [out] if non-NULL, will be set to the line to which the given
 * source location points.
 *
 * \param column [out] if non-NULL, will be set to the column to which the given
 * source location points.
 *
 * \param offset [out] if non-NULL, will be set to the offset into the
 * buffer to which the given source location points.
 *)
procedure clang_getSpellingLocation(location: TCXSourceLocation; _file: PCXFile; line: PCardinal; column: PCardinal; offset: PCardinal); cdecl external LIBCLANG;

(**
 * Retrieve the file, line, column, and offset represented by
 * the given source location.
 *
 * If the location refers into a macro expansion, return where the macro was
 * expanded or where the macro argument was written, if the location points at
 * a macro argument.
 *
 * \param location the location within a source file that will be decomposed
 * into its parts.
 *
 * \param file [out] if non-NULL, will be set to the file to which the given
 * source location points.
 *
 * \param line [out] if non-NULL, will be set to the line to which the given
 * source location points.
 *
 * \param column [out] if non-NULL, will be set to the column to which the given
 * source location points.
 *
 * \param offset [out] if non-NULL, will be set to the offset into the
 * buffer to which the given source location points.
 *)
procedure clang_getFileLocation(location: TCXSourceLocation; _file: PCXFile; line: PCardinal; column: PCardinal; offset: PCardinal); cdecl external LIBCLANG;

(**
 * Retrieve a source location representing the first character within a
 * source range.
 *)
function clang_getRangeStart(range: TCXSourceRange): TCXSourceLocation; cdecl external LIBCLANG;

(**
 * Retrieve a source location representing the last character within a
 * source range.
 *)
function clang_getRangeEnd(range: TCXSourceRange): TCXSourceLocation; cdecl external LIBCLANG;

(**
 * Identifies an array of ranges.
 *)
type
  TCXSourceRangeList = record
    count: Cardinal;
    ranges: PCXSourceRange;
  end;
  PCXSourceRangeList = ^TCXSourceRangeList;

(**
 * Retrieve all ranges that were skipped by the preprocessor.
 *
 * The preprocessor will skip lines when they are surrounded by an
 * if/ifdef/ifndef directive whose condition does not evaluate to true.
 *)
function clang_getSkippedRanges(tu: TCXTranslationUnit; _file: TCXFile): PCXSourceRangeList; cdecl external LIBCLANG;

(**
 * Retrieve all ranges from all files that were skipped by the
 * preprocessor.
 *
 * The preprocessor will skip lines when they are surrounded by an
 * if/ifdef/ifndef directive whose condition does not evaluate to true.
 *)
function clang_getAllSkippedRanges(tu: TCXTranslationUnit): PCXSourceRangeList; cdecl external LIBCLANG;

(**
 * Destroy the given \c CXSourceRangeList.
 *)
procedure clang_disposeSourceRangeList(ranges: PCXSourceRangeList); cdecl external LIBCLANG;

(**
 * @}
 *)

(**
 * \defgroup CINDEX_DIAG Diagnostic reporting
 *
 * @{
 *)

(**
 * Describes the severity of a particular diagnostic.
 *)
type
  TCXDiagnosticSeverity = Integer;

const
  CXDiagnostic_Ignored = 0;
  CXDiagnostic_Note = 1;
  CXDiagnostic_Warning = 2;
  CXDiagnostic_Error = 3;
  CXDiagnostic_Fatal = 4;

(**
 * A single diagnostic, containing the diagnostic's severity,
 * location, text, source ranges, and fix-it hints.
 *)
type TCXDiagnostic = Pointer;

(**
 * A group of CXDiagnostics.
 *)
type TCXDiagnosticSet = Pointer;

(**
 * Determine the number of diagnostics in a CXDiagnosticSet.
 *)
function clang_getNumDiagnosticsInSet(Diags: TCXDiagnosticSet): Cardinal; cdecl external LIBCLANG;

(**
 * Retrieve a diagnostic associated with the given CXDiagnosticSet.
 *
 * \param Diags the CXDiagnosticSet to query.
 * \param Index the zero-based diagnostic number to retrieve.
 *
 * \returns the requested diagnostic. This diagnostic must be freed
 * via a call to \c clang_disposeDiagnostic().
 *)
function clang_getDiagnosticInSet(Diags: TCXDiagnosticSet; Index: Cardinal): TCXDiagnostic; cdecl external LIBCLANG;

(**
 * Describes the kind of error that occurred (if any) in a call to
 * \c clang_loadDiagnostics.
 *)
type
  TCXLoadDiag_Error = Integer;
  PCXLoadDiag_Error = ^TCXLoadDiag_Error;

const
  CXLoadDiag_None = 0;
  CXLoadDiag_Unknown = 1;
  CXLoadDiag_CannotLoad = 2;
  CXLoadDiag_InvalidFile = 3;

(**
 * Deserialize a set of diagnostics from a Clang diagnostics bitcode
 * file.
 *
 * \param file The name of the file to deserialize.
 * \param error A pointer to a enum value recording if there was a problem
 *        deserializing the diagnostics.
 * \param errorString A pointer to a CXString for recording the error string
 *        if the file was not successfully loaded.
 *
 * \returns A loaded CXDiagnosticSet if successful, and NULL otherwise.  These
 * diagnostics should be released using clang_disposeDiagnosticSet().
 *)
function clang_loadDiagnostics(const _file: PAnsiChar; error: PCXLoadDiag_Error; errorString: PCXString): TCXDiagnosticSet; cdecl external LIBCLANG;

(**
 * Release a CXDiagnosticSet and all of its contained diagnostics.
 *)
procedure clang_disposeDiagnosticSet(Diags: TCXDiagnosticSet); cdecl external LIBCLANG;

(**
 * Retrieve the child diagnostics of a CXDiagnostic.
 *
 * This CXDiagnosticSet does not need to be released by
 * clang_disposeDiagnosticSet.
 *)
function clang_getChildDiagnostics(D: TCXDiagnostic): TCXDiagnosticSet; cdecl external LIBCLANG;

(**
 * Determine the number of diagnostics produced for the given
 * translation unit.
 *)
function clang_getNumDiagnostics(_Unit: TCXTranslationUnit): Cardinal; cdecl external LIBCLANG;

(**
 * Retrieve a diagnostic associated with the given translation unit.
 *
 * \param Unit the translation unit to query.
 * \param Index the zero-based diagnostic number to retrieve.
 *
 * \returns the requested diagnostic. This diagnostic must be freed
 * via a call to \c clang_disposeDiagnostic().
 *)
function clang_getDiagnostic(_Unit: TCXTranslationUnit; Index: Cardinal): TCXDiagnostic; cdecl external LIBCLANG;

(**
 * Retrieve the complete set of diagnostics associated with a
 *        translation unit.
 *
 * \param Unit the translation unit to query.
 *)
function clang_getDiagnosticSetFromTU(_Unit: TCXTranslationUnit): TCXDiagnosticSet; cdecl external LIBCLANG;

(**
 * Destroy a diagnostic.
 *)
procedure clang_disposeDiagnostic(Diagnostic: TCXDiagnostic); cdecl external LIBCLANG;

(**
 * Options to control the display of diagnostics.
 *
 * The values in this enum are meant to be combined to customize the
 * behavior of \c clang_formatDiagnostic().
 *)
type
  TCXDiagnosticDisplayOptions = Integer;

const
  CXDiagnostic_DisplaySourceLocation = $01;
  CXDiagnostic_DisplayColumn = $02;
  CXDiagnostic_DisplaySourceRanges = $04;
  CXDiagnostic_DisplayOption = $08;
  CXDiagnostic_DisplayCategoryId = $10;
  CXDiagnostic_DisplayCategoryName = $20;

(**
 * Format the given diagnostic in a manner that is suitable for display.
 *
 * This routine will format the given diagnostic to a string, rendering
 * the diagnostic according to the various options given. The
 * \c clang_defaultDiagnosticDisplayOptions() function returns the set of
 * options that most closely mimics the behavior of the clang compiler.
 *
 * \param Diagnostic The diagnostic to print.
 *
 * \param Options A set of options that control the diagnostic display,
 * created by combining \c CXDiagnosticDisplayOptions values.
 *
 * \returns A new string containing for formatted diagnostic.
 *)
function clang_formatDiagnostic(Diagnostic: TCXDiagnostic; Options: Cardinal): TCXString; cdecl external LIBCLANG;

(**
 * Retrieve the set of display options most similar to the
 * default behavior of the clang compiler.
 *
 * \returns A set of display options suitable for use with \c
 * clang_formatDiagnostic().
 *)
function clang_defaultDiagnosticDisplayOptions(): Cardinal; cdecl external LIBCLANG;

(**
 * Determine the severity of the given diagnostic.
 *)
function clang_getDiagnosticSeverity(p1: TCXDiagnostic): TCXDiagnosticSeverity; cdecl external LIBCLANG;

(**
 * Retrieve the source location of the given diagnostic.
 *
 * This location is where Clang would print the caret ('^') when
 * displaying the diagnostic on the command line.
 *)
function clang_getDiagnosticLocation(p1: TCXDiagnostic): TCXSourceLocation; cdecl external LIBCLANG;

(**
 * Retrieve the text of the given diagnostic.
 *)
function clang_getDiagnosticSpelling(p1: TCXDiagnostic): TCXString; cdecl external LIBCLANG;

(**
 * Retrieve the name of the command-line option that enabled this
 * diagnostic.
 *
 * \param Diag The diagnostic to be queried.
 *
 * \param Disable If non-NULL, will be set to the option that disables this
 * diagnostic (if any).
 *
 * \returns A string that contains the command-line option used to enable this
 * warning, such as "-Wconversion" or "-pedantic".
 *)
function clang_getDiagnosticOption(Diag: TCXDiagnostic; Disable: PCXString): TCXString; cdecl external LIBCLANG;

(**
 * Retrieve the category number for this diagnostic.
 *
 * Diagnostics can be categorized into groups along with other, related
 * diagnostics (e.g., diagnostics under the same warning flag). This routine
 * retrieves the category number for the given diagnostic.
 *
 * \returns The number of the category that contains this diagnostic, or zero
 * if this diagnostic is uncategorized.
 *)
function clang_getDiagnosticCategory(p1: TCXDiagnostic): Cardinal; cdecl external LIBCLANG;

(**
 * Retrieve the name of a particular diagnostic category.  This
 *  is now deprecated.  Use clang_getDiagnosticCategoryText()
 *  instead.
 *
 * \param Category A diagnostic category number, as returned by
 * \c clang_getDiagnosticCategory().
 *
 * \returns The name of the given diagnostic category.
 *)
function clang_getDiagnosticCategoryName(Category: Cardinal): TCXString; cdecl external LIBCLANG;

(**
 * Retrieve the diagnostic category text for a given diagnostic.
 *
 * \returns The text of the given diagnostic category.
 *)
function clang_getDiagnosticCategoryText(p1: TCXDiagnostic): TCXString; cdecl external LIBCLANG;

(**
 * Determine the number of source ranges associated with the given
 * diagnostic.
 *)
function clang_getDiagnosticNumRanges(p1: TCXDiagnostic): Cardinal; cdecl external LIBCLANG;

(**
 * Retrieve a source range associated with the diagnostic.
 *
 * A diagnostic's source ranges highlight important elements in the source
 * code. On the command line, Clang displays source ranges by
 * underlining them with '~' characters.
 *
 * \param Diagnostic the diagnostic whose range is being extracted.
 *
 * \param Range the zero-based index specifying which range to
 *
 * \returns the requested source range.
 *)
function clang_getDiagnosticRange(Diagnostic: TCXDiagnostic; Range: Cardinal): TCXSourceRange; cdecl external LIBCLANG;

(**
 * Determine the number of fix-it hints associated with the
 * given diagnostic.
 *)
function clang_getDiagnosticNumFixIts(Diagnostic: TCXDiagnostic): Cardinal; cdecl external LIBCLANG;

(**
 * Retrieve the replacement information for a given fix-it.
 *
 * Fix-its are described in terms of a source range whose contents
 * should be replaced by a string. This approach generalizes over
 * three kinds of operations: removal of source code (the range covers
 * the code to be removed and the replacement string is empty),
 * replacement of source code (the range covers the code to be
 * replaced and the replacement string provides the new code), and
 * insertion (both the start and end of the range point at the
 * insertion location, and the replacement string provides the text to
 * insert).
 *
 * \param Diagnostic The diagnostic whose fix-its are being queried.
 *
 * \param FixIt The zero-based index of the fix-it.
 *
 * \param ReplacementRange The source range whose contents will be
 * replaced with the returned replacement string. Note that source
 * ranges are half-open ranges [a, b), so the source code should be
 * replaced from a and up to (but not including) b.
 *
 * \returns A string containing text that should be replace the source
 * code indicated by the \c ReplacementRange.
 *)
function clang_getDiagnosticFixIt(Diagnostic: TCXDiagnostic; FixIt: Cardinal; ReplacementRange: PCXSourceRange): TCXString; cdecl external LIBCLANG;

(**
 * @}
 *)

(**
 * \defgroup CINDEX_TRANSLATION_UNIT Translation unit manipulation
 *
 * The routines in this group provide the ability to create and destroy
 * translation units from files, either by parsing the contents of the files or
 * by reading in a serialized representation of a translation unit.
 *
 * @{
 *)

(**
 * Get the original translation unit source file name.
 *)
function clang_getTranslationUnitSpelling(CTUnit: TCXTranslationUnit): TCXString; cdecl external LIBCLANG;

(**
 * Return the CXTranslationUnit for a given source file and the provided
 * command line arguments one would pass to the compiler.
 *
 * Note: The 'source_filename' argument is optional.  If the caller provides a
 * NULL pointer, the name of the source file is expected to reside in the
 * specified command line arguments.
 *
 * Note: When encountered in 'clang_command_line_args', the following options
 * are ignored:
 *
 *   '-c'
 *   '-emit-ast'
 *   '-fsyntax-only'
 *   '-o \<output file>'  (both '-o' and '\<output file>' are ignored)
 *
 * \param CIdx The index object with which the translation unit will be
 * associated.
 *
 * \param source_filename The name of the source file to load, or NULL if the
 * source file is included in \p clang_command_line_args.
 *
 * \param num_clang_command_line_args The number of command-line arguments in
 * \p clang_command_line_args.
 *
 * \param clang_command_line_args The command-line arguments that would be
 * passed to the \c clang executable if it were being invoked out-of-process.
 * These command-line options will be parsed and will affect how the translation
 * unit is parsed. Note that the following options are ignored: '-c',
 * '-emit-ast', '-fsyntax-only' (which is the default), and '-o \<output file>'.
 *
 * \param num_unsaved_files the number of unsaved file entries in \p
 * unsaved_files.
 *
 * \param unsaved_files the files that have not yet been saved to disk
 * but may be required for code completion, including the contents of
 * those files.  The contents and name of these files (as specified by
 * CXUnsavedFile) are copied when necessary, so the client only needs to
 * guarantee their validity until the call to this function returns.
 *)
function clang_createTranslationUnitFromSourceFile(CIdx: TCXIndex; const source_filename: PAnsiChar; num_clang_command_line_args: Integer; const clang_command_line_args: PPAnsiChar; num_unsaved_files: Cardinal; unsaved_files: PCXUnsavedFile): TCXTranslationUnit; cdecl external LIBCLANG;

(**
 * Same as \c clang_createTranslationUnit2, but returns
 * the \c CXTranslationUnit instead of an error code.  In case of an error this
 * routine returns a \c NULL \c CXTranslationUnit, without further detailed
 * error codes.
 *)
function clang_createTranslationUnit(CIdx: TCXIndex; const ast_filename: PAnsiChar): TCXTranslationUnit; cdecl external LIBCLANG;

(**
 * Create a translation unit from an AST file (\c -emit-ast).
 *
 * \param[out] out_TU A non-NULL pointer to store the created
 * \c CXTranslationUnit.
 *
 * \returns Zero on success, otherwise returns an error code.
 *)
function clang_createTranslationUnit2(CIdx: TCXIndex; const ast_filename: PAnsiChar; out_TU: PCXTranslationUnit): TCXErrorCode; cdecl external LIBCLANG;

(**
 * Flags that control the creation of translation units.
 *
 * The enumerators in this enumeration type are meant to be bitwise
 * ORed together to specify which options should be used when
 * constructing the translation unit.
 *)
type
  TCXTranslationUnit_Flags = Integer;

const
  CXTranslationUnit_None = $0;
  CXTranslationUnit_DetailedPreprocessingRecord = $01;
  CXTranslationUnit_Incomplete = $02;
  CXTranslationUnit_PrecompiledPreamble = $04;
  CXTranslationUnit_CacheCompletionResults = $08;
  CXTranslationUnit_ForSerialization = $10;
  CXTranslationUnit_CXXChainedPCH = $20;
  CXTranslationUnit_SkipFunctionBodies = $40;
  CXTranslationUnit_IncludeBriefCommentsInCodeCompletion = $80;
  CXTranslationUnit_CreatePreambleOnFirstParse = $100;
  CXTranslationUnit_KeepGoing = $200;
  CXTranslationUnit_SingleFileParse = $400;
  CXTranslationUnit_LimitSkipFunctionBodiesToPreamble = $800;
  CXTranslationUnit_IncludeAttributedTypes = $1000;
  CXTranslationUnit_VisitImplicitAttributes = $2000;
  CXTranslationUnit_IgnoreNonErrorsFromIncludedFiles = $4000;

(**
 * Returns the set of flags that is suitable for parsing a translation
 * unit that is being edited.
 *
 * The set of flags returned provide options for \c clang_parseTranslationUnit()
 * to indicate that the translation unit is likely to be reparsed many times,
 * either explicitly (via \c clang_reparseTranslationUnit()) or implicitly
 * (e.g., by code completion (\c clang_codeCompletionAt())). The returned flag
 * set contains an unspecified set of optimizations (e.g., the precompiled
 * preamble) geared toward improving the performance of these routines. The
 * set of optimizations enabled may change from one version to the next.
 *)
function clang_defaultEditingTranslationUnitOptions(): Cardinal; cdecl external LIBCLANG;

(**
 * Same as \c clang_parseTranslationUnit2, but returns
 * the \c CXTranslationUnit instead of an error code.  In case of an error this
 * routine returns a \c NULL \c CXTranslationUnit, without further detailed
 * error codes.
 *)
function clang_parseTranslationUnit(CIdx: TCXIndex; const source_filename: PAnsiChar; const command_line_args: PPAnsiChar; num_command_line_args: Integer; unsaved_files: PCXUnsavedFile; num_unsaved_files: Cardinal; options: Cardinal): TCXTranslationUnit; cdecl external LIBCLANG;

(**
 * Parse the given source file and the translation unit corresponding
 * to that file.
 *
 * This routine is the main entry point for the Clang C API, providing the
 * ability to parse a source file into a translation unit that can then be
 * queried by other functions in the API. This routine accepts a set of
 * command-line arguments so that the compilation can be configured in the same
 * way that the compiler is configured on the command line.
 *
 * \param CIdx The index object with which the translation unit will be
 * associated.
 *
 * \param source_filename The name of the source file to load, or NULL if the
 * source file is included in \c command_line_args.
 *
 * \param command_line_args The command-line arguments that would be
 * passed to the \c clang executable if it were being invoked out-of-process.
 * These command-line options will be parsed and will affect how the translation
 * unit is parsed. Note that the following options are ignored: '-c',
 * '-emit-ast', '-fsyntax-only' (which is the default), and '-o \<output file>'.
 *
 * \param num_command_line_args The number of command-line arguments in
 * \c command_line_args.
 *
 * \param unsaved_files the files that have not yet been saved to disk
 * but may be required for parsing, including the contents of
 * those files.  The contents and name of these files (as specified by
 * CXUnsavedFile) are copied when necessary, so the client only needs to
 * guarantee their validity until the call to this function returns.
 *
 * \param num_unsaved_files the number of unsaved file entries in \p
 * unsaved_files.
 *
 * \param options A bitmask of options that affects how the translation unit
 * is managed but not its compilation. This should be a bitwise OR of the
 * CXTranslationUnit_XXX flags.
 *
 * \param[out] out_TU A non-NULL pointer to store the created
 * \c CXTranslationUnit, describing the parsed code and containing any
 * diagnostics produced by the compiler.
 *
 * \returns Zero on success, otherwise returns an error code.
 *)
function clang_parseTranslationUnit2(CIdx: TCXIndex; const source_filename: PAnsiChar; const command_line_args: PPAnsiChar; num_command_line_args: Integer; unsaved_files: PCXUnsavedFile; num_unsaved_files: Cardinal; options: Cardinal; out_TU: PCXTranslationUnit): TCXErrorCode; cdecl external LIBCLANG;

(**
 * Same as clang_parseTranslationUnit2 but requires a full command line
 * for \c command_line_args including argv[0]. This is useful if the standard
 * library paths are relative to the binary.
 *)
function clang_parseTranslationUnit2FullArgv(CIdx: TCXIndex; const source_filename: PAnsiChar; const command_line_args: PPAnsiChar; num_command_line_args: Integer; unsaved_files: PCXUnsavedFile; num_unsaved_files: Cardinal; options: Cardinal; out_TU: PCXTranslationUnit): TCXErrorCode; cdecl external LIBCLANG;

(**
 * Flags that control how translation units are saved.
 *
 * The enumerators in this enumeration type are meant to be bitwise
 * ORed together to specify which options should be used when
 * saving the translation unit.
 *)
type
  TCXSaveTranslationUnit_Flags = Integer;

const
  CXSaveTranslationUnit_None = $0;

(**
 * Returns the set of flags that is suitable for saving a translation
 * unit.
 *
 * The set of flags returned provide options for
 * \c clang_saveTranslationUnit() by default. The returned flag
 * set contains an unspecified set of options that save translation units with
 * the most commonly-requested data.
 *)
function clang_defaultSaveOptions(TU: TCXTranslationUnit): Cardinal; cdecl external LIBCLANG;

(**
 * Describes the kind of error that occurred (if any) in a call to
 * \c clang_saveTranslationUnit().
 *)
type
  TCXSaveError = Integer;

const
  CXSaveError_None = 0;
  CXSaveError_Unknown = 1;
  CXSaveError_TranslationErrors = 2;
  CXSaveError_InvalidTU = 3;

(**
 * Saves a translation unit into a serialized representation of
 * that translation unit on disk.
 *
 * Any translation unit that was parsed without error can be saved
 * into a file. The translation unit can then be deserialized into a
 * new \c CXTranslationUnit with \c clang_createTranslationUnit() or,
 * if it is an incomplete translation unit that corresponds to a
 * header, used as a precompiled header when parsing other translation
 * units.
 *
 * \param TU The translation unit to save.
 *
 * \param FileName The file to which the translation unit will be saved.
 *
 * \param options A bitmask of options that affects how the translation unit
 * is saved. This should be a bitwise OR of the
 * CXSaveTranslationUnit_XXX flags.
 *
 * \returns A value that will match one of the enumerators of the CXSaveError
 * enumeration. Zero (CXSaveError_None) indicates that the translation unit was
 * saved successfully, while a non-zero value indicates that a problem occurred.
 *)
function clang_saveTranslationUnit(TU: TCXTranslationUnit; const FileName: PAnsiChar; options: Cardinal): Integer; cdecl external LIBCLANG;

(**
 * Suspend a translation unit in order to free memory associated with it.
 *
 * A suspended translation unit uses significantly less memory but on the other
 * side does not support any other calls than \c clang_reparseTranslationUnit
 * to resume it or \c clang_disposeTranslationUnit to dispose it completely.
 *)
function clang_suspendTranslationUnit(p1: TCXTranslationUnit): Cardinal; cdecl external LIBCLANG;

(**
 * Destroy the specified CXTranslationUnit object.
 *)
procedure clang_disposeTranslationUnit(p1: TCXTranslationUnit); cdecl external LIBCLANG;

(**
 * Flags that control the reparsing of translation units.
 *
 * The enumerators in this enumeration type are meant to be bitwise
 * ORed together to specify which options should be used when
 * reparsing the translation unit.
 *)
type
  TCXReparse_Flags = Integer;

const
  CXReparse_None = $0;

(**
 * Returns the set of flags that is suitable for reparsing a translation
 * unit.
 *
 * The set of flags returned provide options for
 * \c clang_reparseTranslationUnit() by default. The returned flag
 * set contains an unspecified set of optimizations geared toward common uses
 * of reparsing. The set of optimizations enabled may change from one version
 * to the next.
 *)
function clang_defaultReparseOptions(TU: TCXTranslationUnit): Cardinal; cdecl external LIBCLANG;

(**
 * Reparse the source files that produced this translation unit.
 *
 * This routine can be used to re-parse the source files that originally
 * created the given translation unit, for example because those source files
 * have changed (either on disk or as passed via \p unsaved_files). The
 * source code will be reparsed with the same command-line options as it
 * was originally parsed.
 *
 * Reparsing a translation unit invalidates all cursors and source locations
 * that refer into that translation unit. This makes reparsing a translation
 * unit semantically equivalent to destroying the translation unit and then
 * creating a new translation unit with the same command-line arguments.
 * However, it may be more efficient to reparse a translation
 * unit using this routine.
 *
 * \param TU The translation unit whose contents will be re-parsed. The
 * translation unit must originally have been built with
 * \c clang_createTranslationUnitFromSourceFile().
 *
 * \param num_unsaved_files The number of unsaved file entries in \p
 * unsaved_files.
 *
 * \param unsaved_files The files that have not yet been saved to disk
 * but may be required for parsing, including the contents of
 * those files.  The contents and name of these files (as specified by
 * CXUnsavedFile) are copied when necessary, so the client only needs to
 * guarantee their validity until the call to this function returns.
 *
 * \param options A bitset of options composed of the flags in CXReparse_Flags.
 * The function \c clang_defaultReparseOptions() produces a default set of
 * options recommended for most uses, based on the translation unit.
 *
 * \returns 0 if the sources could be reparsed.  A non-zero error code will be
 * returned if reparsing was impossible, such that the translation unit is
 * invalid. In such cases, the only valid call for \c TU is
 * \c clang_disposeTranslationUnit(TU).  The error codes returned by this
 * routine are described by the \c CXErrorCode enum.
 *)
function clang_reparseTranslationUnit(TU: TCXTranslationUnit; num_unsaved_files: Cardinal; unsaved_files: PCXUnsavedFile; options: Cardinal): Integer; cdecl external LIBCLANG;

(**
  * Categorizes how memory is being used by a translation unit.
  *)
type
  TCXTUResourceUsageKind = Integer;

const
  CXTUResourceUsage_AST = 1;
  CXTUResourceUsage_Identifiers = 2;
  CXTUResourceUsage_Selectors = 3;
  CXTUResourceUsage_GlobalCompletionResults = 4;
  CXTUResourceUsage_SourceManagerContentCache = 5;
  CXTUResourceUsage_AST_SideTables = 6;
  CXTUResourceUsage_SourceManager_Membuffer_Malloc = 7;
  CXTUResourceUsage_SourceManager_Membuffer_MMap = 8;
  CXTUResourceUsage_ExternalASTSource_Membuffer_Malloc = 9;
  CXTUResourceUsage_ExternalASTSource_Membuffer_MMap = 10;
  CXTUResourceUsage_Preprocessor = 11;
  CXTUResourceUsage_PreprocessingRecord = 12;
  CXTUResourceUsage_SourceManager_DataStructures = 13;
  CXTUResourceUsage_Preprocessor_HeaderSearch = 14;
  CXTUResourceUsage_MEMORY_IN_BYTES_BEGIN = CXTUResourceUsage_AST;
  CXTUResourceUsage_MEMORY_IN_BYTES_END = CXTUResourceUsage_Preprocessor_HeaderSearch;
  CXTUResourceUsage_First = CXTUResourceUsage_AST;
  CXTUResourceUsage_Last = CXTUResourceUsage_Preprocessor_HeaderSearch;

(**
  * Returns the human-readable null-terminated C string that represents
  *  the name of the memory category.  This string should never be freed.
  *)
function clang_getTUResourceUsageName(kind: TCXTUResourceUsageKind): PAnsiChar; cdecl external LIBCLANG;

type
  TCXTUResourceUsageEntry = record
    kind: TCXTUResourceUsageKind;
    amount: Longword;
  end;
  PCXTUResourceUsageEntry = ^TCXTUResourceUsageEntry;

(**
  * The memory usage of a CXTranslationUnit, broken into categories.
  *)
type
  TCXTUResourceUsage = record
    data: Pointer;
    numEntries: Cardinal;
    entries: PCXTUResourceUsageEntry;
  end;
  PCXTUResourceUsage = ^TCXTUResourceUsage;

(**
  * Return the memory usage of a translation unit.  This object
  *  should be released with clang_disposeCXTUResourceUsage().
  *)
function clang_getCXTUResourceUsage(TU: TCXTranslationUnit): TCXTUResourceUsage; cdecl external LIBCLANG;

procedure clang_disposeCXTUResourceUsage(usage: TCXTUResourceUsage); cdecl external LIBCLANG;

(**
 * Get target information for this translation unit.
 *
 * The CXTargetInfo object cannot outlive the CXTranslationUnit object.
 *)
function clang_getTranslationUnitTargetInfo(CTUnit: TCXTranslationUnit): TCXTargetInfo; cdecl external LIBCLANG;

(**
 * Destroy the CXTargetInfo object.
 *)
procedure clang_TargetInfo_dispose(Info: TCXTargetInfo); cdecl external LIBCLANG;

(**
 * Get the normalized target triple as a string.
 *
 * Returns the empty string in case of any error.
 *)
function clang_TargetInfo_getTriple(Info: TCXTargetInfo): TCXString; cdecl external LIBCLANG;

(**
 * Get the pointer width of the target in bits.
 *
 * Returns -1 in case of error.
 *)
function clang_TargetInfo_getPointerWidth(Info: TCXTargetInfo): Integer; cdecl external LIBCLANG;

(**
 * @}
 *)

(**
 * Describes the kind of entity that a cursor refers to.
 *)
type
  TCXCursorKind = Integer;
  PCXCursorKind = ^TCXCursorKind;

const
  CXCursor_UnexposedDecl = 1;
  CXCursor_StructDecl = 2;
  CXCursor_UnionDecl = 3;
  CXCursor_ClassDecl = 4;
  CXCursor_EnumDecl = 5;
  CXCursor_FieldDecl = 6;
  CXCursor_EnumConstantDecl = 7;
  CXCursor_FunctionDecl = 8;
  CXCursor_VarDecl = 9;
  CXCursor_ParmDecl = 10;
  CXCursor_ObjCInterfaceDecl = 11;
  CXCursor_ObjCCategoryDecl = 12;
  CXCursor_ObjCProtocolDecl = 13;
  CXCursor_ObjCPropertyDecl = 14;
  CXCursor_ObjCIvarDecl = 15;
  CXCursor_ObjCInstanceMethodDecl = 16;
  CXCursor_ObjCClassMethodDecl = 17;
  CXCursor_ObjCImplementationDecl = 18;
  CXCursor_ObjCCategoryImplDecl = 19;
  CXCursor_TypedefDecl = 20;
  CXCursor_CXXMethod = 21;
  CXCursor_Namespace = 22;
  CXCursor_LinkageSpec = 23;
  CXCursor_Constructor = 24;
  CXCursor_Destructor = 25;
  CXCursor_ConversionFunction = 26;
  CXCursor_TemplateTypeParameter = 27;
  CXCursor_NonTypeTemplateParameter = 28;
  CXCursor_TemplateTemplateParameter = 29;
  CXCursor_FunctionTemplate = 30;
  CXCursor_ClassTemplate = 31;
  CXCursor_ClassTemplatePartialSpecialization = 32;
  CXCursor_NamespaceAlias = 33;
  CXCursor_UsingDirective = 34;
  CXCursor_UsingDeclaration = 35;
  CXCursor_TypeAliasDecl = 36;
  CXCursor_ObjCSynthesizeDecl = 37;
  CXCursor_ObjCDynamicDecl = 38;
  CXCursor_CXXAccessSpecifier = 39;
  CXCursor_FirstDecl = CXCursor_UnexposedDecl;
  CXCursor_LastDecl = CXCursor_CXXAccessSpecifier;
  CXCursor_FirstRef = 40;
  CXCursor_ObjCSuperClassRef = 40;
  CXCursor_ObjCProtocolRef = 41;
  CXCursor_ObjCClassRef = 42;
  CXCursor_TypeRef = 43;
  CXCursor_CXXBaseSpecifier = 44;
  CXCursor_TemplateRef = 45;
  CXCursor_NamespaceRef = 46;
  CXCursor_MemberRef = 47;
  CXCursor_LabelRef = 48;
  CXCursor_OverloadedDeclRef = 49;
  CXCursor_VariableRef = 50;
  CXCursor_LastRef = CXCursor_VariableRef;
  CXCursor_FirstInvalid = 70;
  CXCursor_InvalidFile = 70;
  CXCursor_NoDeclFound = 71;
  CXCursor_NotImplemented = 72;
  CXCursor_InvalidCode = 73;
  CXCursor_LastInvalid = CXCursor_InvalidCode;
  CXCursor_FirstExpr = 100;
  CXCursor_UnexposedExpr = 100;
  CXCursor_DeclRefExpr = 101;
  CXCursor_MemberRefExpr = 102;
  CXCursor_CallExpr = 103;
  CXCursor_ObjCMessageExpr = 104;
  CXCursor_BlockExpr = 105;
  CXCursor_IntegerLiteral = 106;
  CXCursor_FloatingLiteral = 107;
  CXCursor_ImaginaryLiteral = 108;
  CXCursor_StringLiteral = 109;
  CXCursor_CharacterLiteral = 110;
  CXCursor_ParenExpr = 111;
  CXCursor_UnaryOperator = 112;
  CXCursor_ArraySubscriptExpr = 113;
  CXCursor_BinaryOperator = 114;
  CXCursor_CompoundAssignOperator = 115;
  CXCursor_ConditionalOperator = 116;
  CXCursor_CStyleCastExpr = 117;
  CXCursor_CompoundLiteralExpr = 118;
  CXCursor_InitListExpr = 119;
  CXCursor_AddrLabelExpr = 120;
  CXCursor_StmtExpr = 121;
  CXCursor_GenericSelectionExpr = 122;
  CXCursor_GNUNullExpr = 123;
  CXCursor_CXXStaticCastExpr = 124;
  CXCursor_CXXDynamicCastExpr = 125;
  CXCursor_CXXReinterpretCastExpr = 126;
  CXCursor_CXXConstCastExpr = 127;
  CXCursor_CXXFunctionalCastExpr = 128;
  CXCursor_CXXTypeidExpr = 129;
  CXCursor_CXXBoolLiteralExpr = 130;
  CXCursor_CXXNullPtrLiteralExpr = 131;
  CXCursor_CXXThisExpr = 132;
  CXCursor_CXXThrowExpr = 133;
  CXCursor_CXXNewExpr = 134;
  CXCursor_CXXDeleteExpr = 135;
  CXCursor_UnaryExpr = 136;
  CXCursor_ObjCStringLiteral = 137;
  CXCursor_ObjCEncodeExpr = 138;
  CXCursor_ObjCSelectorExpr = 139;
  CXCursor_ObjCProtocolExpr = 140;
  CXCursor_ObjCBridgedCastExpr = 141;
  CXCursor_PackExpansionExpr = 142;
  CXCursor_SizeOfPackExpr = 143;
  CXCursor_LambdaExpr = 144;
  CXCursor_ObjCBoolLiteralExpr = 145;
  CXCursor_ObjCSelfExpr = 146;
  CXCursor_OMPArraySectionExpr = 147;
  CXCursor_ObjCAvailabilityCheckExpr = 148;
  CXCursor_FixedPointLiteral = 149;
  CXCursor_LastExpr = CXCursor_FixedPointLiteral;
  CXCursor_FirstStmt = 200;
  CXCursor_UnexposedStmt = 200;
  CXCursor_LabelStmt = 201;
  CXCursor_CompoundStmt = 202;
  CXCursor_CaseStmt = 203;
  CXCursor_DefaultStmt = 204;
  CXCursor_IfStmt = 205;
  CXCursor_SwitchStmt = 206;
  CXCursor_WhileStmt = 207;
  CXCursor_DoStmt = 208;
  CXCursor_ForStmt = 209;
  CXCursor_GotoStmt = 210;
  CXCursor_IndirectGotoStmt = 211;
  CXCursor_ContinueStmt = 212;
  CXCursor_BreakStmt = 213;
  CXCursor_ReturnStmt = 214;
  CXCursor_GCCAsmStmt = 215;
  CXCursor_AsmStmt = CXCursor_GCCAsmStmt;
  CXCursor_ObjCAtTryStmt = 216;
  CXCursor_ObjCAtCatchStmt = 217;
  CXCursor_ObjCAtFinallyStmt = 218;
  CXCursor_ObjCAtThrowStmt = 219;
  CXCursor_ObjCAtSynchronizedStmt = 220;
  CXCursor_ObjCAutoreleasePoolStmt = 221;
  CXCursor_ObjCForCollectionStmt = 222;
  CXCursor_CXXCatchStmt = 223;
  CXCursor_CXXTryStmt = 224;
  CXCursor_CXXForRangeStmt = 225;
  CXCursor_SEHTryStmt = 226;
  CXCursor_SEHExceptStmt = 227;
  CXCursor_SEHFinallyStmt = 228;
  CXCursor_MSAsmStmt = 229;
  CXCursor_NullStmt = 230;
  CXCursor_DeclStmt = 231;
  CXCursor_OMPParallelDirective = 232;
  CXCursor_OMPSimdDirective = 233;
  CXCursor_OMPForDirective = 234;
  CXCursor_OMPSectionsDirective = 235;
  CXCursor_OMPSectionDirective = 236;
  CXCursor_OMPSingleDirective = 237;
  CXCursor_OMPParallelForDirective = 238;
  CXCursor_OMPParallelSectionsDirective = 239;
  CXCursor_OMPTaskDirective = 240;
  CXCursor_OMPMasterDirective = 241;
  CXCursor_OMPCriticalDirective = 242;
  CXCursor_OMPTaskyieldDirective = 243;
  CXCursor_OMPBarrierDirective = 244;
  CXCursor_OMPTaskwaitDirective = 245;
  CXCursor_OMPFlushDirective = 246;
  CXCursor_SEHLeaveStmt = 247;
  CXCursor_OMPOrderedDirective = 248;
  CXCursor_OMPAtomicDirective = 249;
  CXCursor_OMPForSimdDirective = 250;
  CXCursor_OMPParallelForSimdDirective = 251;
  CXCursor_OMPTargetDirective = 252;
  CXCursor_OMPTeamsDirective = 253;
  CXCursor_OMPTaskgroupDirective = 254;
  CXCursor_OMPCancellationPointDirective = 255;
  CXCursor_OMPCancelDirective = 256;
  CXCursor_OMPTargetDataDirective = 257;
  CXCursor_OMPTaskLoopDirective = 258;
  CXCursor_OMPTaskLoopSimdDirective = 259;
  CXCursor_OMPDistributeDirective = 260;
  CXCursor_OMPTargetEnterDataDirective = 261;
  CXCursor_OMPTargetExitDataDirective = 262;
  CXCursor_OMPTargetParallelDirective = 263;
  CXCursor_OMPTargetParallelForDirective = 264;
  CXCursor_OMPTargetUpdateDirective = 265;
  CXCursor_OMPDistributeParallelForDirective = 266;
  CXCursor_OMPDistributeParallelForSimdDirective = 267;
  CXCursor_OMPDistributeSimdDirective = 268;
  CXCursor_OMPTargetParallelForSimdDirective = 269;
  CXCursor_OMPTargetSimdDirective = 270;
  CXCursor_OMPTeamsDistributeDirective = 271;
  CXCursor_OMPTeamsDistributeSimdDirective = 272;
  CXCursor_OMPTeamsDistributeParallelForSimdDirective = 273;
  CXCursor_OMPTeamsDistributeParallelForDirective = 274;
  CXCursor_OMPTargetTeamsDirective = 275;
  CXCursor_OMPTargetTeamsDistributeDirective = 276;
  CXCursor_OMPTargetTeamsDistributeParallelForDirective = 277;
  CXCursor_OMPTargetTeamsDistributeParallelForSimdDirective = 278;
  CXCursor_OMPTargetTeamsDistributeSimdDirective = 279;
  CXCursor_BuiltinBitCastExpr = 280;
  CXCursor_LastStmt = CXCursor_BuiltinBitCastExpr;
  CXCursor_TranslationUnit = 300;
  CXCursor_FirstAttr = 400;
  CXCursor_UnexposedAttr = 400;
  CXCursor_IBActionAttr = 401;
  CXCursor_IBOutletAttr = 402;
  CXCursor_IBOutletCollectionAttr = 403;
  CXCursor_CXXFinalAttr = 404;
  CXCursor_CXXOverrideAttr = 405;
  CXCursor_AnnotateAttr = 406;
  CXCursor_AsmLabelAttr = 407;
  CXCursor_PackedAttr = 408;
  CXCursor_PureAttr = 409;
  CXCursor_ConstAttr = 410;
  CXCursor_NoDuplicateAttr = 411;
  CXCursor_CUDAConstantAttr = 412;
  CXCursor_CUDADeviceAttr = 413;
  CXCursor_CUDAGlobalAttr = 414;
  CXCursor_CUDAHostAttr = 415;
  CXCursor_CUDASharedAttr = 416;
  CXCursor_VisibilityAttr = 417;
  CXCursor_DLLExport = 418;
  CXCursor_DLLImport = 419;
  CXCursor_NSReturnsRetained = 420;
  CXCursor_NSReturnsNotRetained = 421;
  CXCursor_NSReturnsAutoreleased = 422;
  CXCursor_NSConsumesSelf = 423;
  CXCursor_NSConsumed = 424;
  CXCursor_ObjCException = 425;
  CXCursor_ObjCNSObject = 426;
  CXCursor_ObjCIndependentClass = 427;
  CXCursor_ObjCPreciseLifetime = 428;
  CXCursor_ObjCReturnsInnerPointer = 429;
  CXCursor_ObjCRequiresSuper = 430;
  CXCursor_ObjCRootClass = 431;
  CXCursor_ObjCSubclassingRestricted = 432;
  CXCursor_ObjCExplicitProtocolImpl = 433;
  CXCursor_ObjCDesignatedInitializer = 434;
  CXCursor_ObjCRuntimeVisible = 435;
  CXCursor_ObjCBoxable = 436;
  CXCursor_FlagEnum = 437;
  CXCursor_ConvergentAttr = 438;
  CXCursor_WarnUnusedAttr = 439;
  CXCursor_WarnUnusedResultAttr = 440;
  CXCursor_AlignedAttr = 441;
  CXCursor_LastAttr = CXCursor_AlignedAttr;
  CXCursor_PreprocessingDirective = 500;
  CXCursor_MacroDefinition = 501;
  CXCursor_MacroExpansion = 502;
  CXCursor_MacroInstantiation = CXCursor_MacroExpansion;
  CXCursor_InclusionDirective = 503;
  CXCursor_FirstPreprocessing = CXCursor_PreprocessingDirective;
  CXCursor_LastPreprocessing = CXCursor_InclusionDirective;
  CXCursor_ModuleImportDecl = 600;
  CXCursor_TypeAliasTemplateDecl = 601;
  CXCursor_StaticAssert = 602;
  CXCursor_FriendDecl = 603;
  CXCursor_FirstExtraDecl = CXCursor_ModuleImportDecl;
  CXCursor_LastExtraDecl = CXCursor_FriendDecl;
  CXCursor_OverloadCandidate = 700;

(**
 * A cursor representing some element in the abstract syntax tree for
 * a translation unit.
 *
 * The cursor abstraction unifies the different kinds of entities in a
 * program--declaration, statements, expressions, references to declarations,
 * etc.--under a single "cursor" abstraction with a common set of operations.
 * Common operation for a cursor include: getting the physical location in
 * a source file where the cursor points, getting the name associated with a
 * cursor, and retrieving cursors for any child nodes of a particular cursor.
 *
 * Cursors can be produced in two specific ways.
 * clang_getTranslationUnitCursor() produces a cursor for a translation unit,
 * from which one can use clang_visitChildren() to explore the rest of the
 * translation unit. clang_getCursor() maps from a physical source location
 * to the entity that resides at that location, allowing one to map from the
 * source code into the AST.
 *)
type
  TCXCursor = record
    kind: TCXCursorKind;
    xdata: Integer;
    data: array [0..3-1] of Pointer;
  end;
  PCXCursor = ^TCXCursor;
  PPCXCursor = ^PCXCursor;

(**
 * \defgroup CINDEX_CURSOR_MANIP Cursor manipulations
 *
 * @{
 *)

(**
 * Retrieve the NULL cursor, which represents no entity.
 *)
function clang_getNullCursor(): TCXCursor; cdecl external LIBCLANG;

(**
 * Retrieve the cursor that represents the given translation unit.
 *
 * The translation unit cursor can be used to start traversing the
 * various declarations within the given translation unit.
 *)
function clang_getTranslationUnitCursor(p1: TCXTranslationUnit): TCXCursor; cdecl external LIBCLANG;

(**
 * Determine whether two cursors are equivalent.
 *)
function clang_equalCursors(p1: TCXCursor; p2: TCXCursor): Cardinal; cdecl external LIBCLANG;

(**
 * Returns non-zero if \p cursor is null.
 *)
function clang_Cursor_isNull(cursor: TCXCursor): Integer; cdecl external LIBCLANG;

(**
 * Compute a hash value for the given cursor.
 *)
function clang_hashCursor(p1: TCXCursor): Cardinal; cdecl external LIBCLANG;

(**
 * Retrieve the kind of the given cursor.
 *)
function clang_getCursorKind(p1: TCXCursor): TCXCursorKind; cdecl external LIBCLANG;

(**
 * Determine whether the given cursor kind represents a declaration.
 *)
function clang_isDeclaration(p1: TCXCursorKind): Cardinal; cdecl external LIBCLANG;

(**
 * Determine whether the given declaration is invalid.
 *
 * A declaration is invalid if it could not be parsed successfully.
 *
 * \returns non-zero if the cursor represents a declaration and it is
 * invalid, otherwise NULL.
 *)
function clang_isInvalidDeclaration(p1: TCXCursor): Cardinal; cdecl external LIBCLANG;

(**
 * Determine whether the given cursor kind represents a simple
 * reference.
 *
 * Note that other kinds of cursors (such as expressions) can also refer to
 * other cursors. Use clang_getCursorReferenced() to determine whether a
 * particular cursor refers to another entity.
 *)
function clang_isReference(p1: TCXCursorKind): Cardinal; cdecl external LIBCLANG;

(**
 * Determine whether the given cursor kind represents an expression.
 *)
function clang_isExpression(p1: TCXCursorKind): Cardinal; cdecl external LIBCLANG;

(**
 * Determine whether the given cursor kind represents a statement.
 *)
function clang_isStatement(p1: TCXCursorKind): Cardinal; cdecl external LIBCLANG;

(**
 * Determine whether the given cursor kind represents an attribute.
 *)
function clang_isAttribute(p1: TCXCursorKind): Cardinal; cdecl external LIBCLANG;

(**
 * Determine whether the given cursor has any attributes.
 *)
function clang_Cursor_hasAttrs(C: TCXCursor): Cardinal; cdecl external LIBCLANG;

(**
 * Determine whether the given cursor kind represents an invalid
 * cursor.
 *)
function clang_isInvalid(p1: TCXCursorKind): Cardinal; cdecl external LIBCLANG;

(**
 * Determine whether the given cursor kind represents a translation
 * unit.
 *)
function clang_isTranslationUnit(p1: TCXCursorKind): Cardinal; cdecl external LIBCLANG;

(***
 * Determine whether the given cursor represents a preprocessing
 * element, such as a preprocessor directive or macro instantiation.
 *)
function clang_isPreprocessing(p1: TCXCursorKind): Cardinal; cdecl external LIBCLANG;

(***
 * Determine whether the given cursor represents a currently
 *  unexposed piece of the AST (e.g., CXCursor_UnexposedStmt).
 *)
function clang_isUnexposed(p1: TCXCursorKind): Cardinal; cdecl external LIBCLANG;

(**
 * Describe the linkage of the entity referred to by a cursor.
 *)
type
  TCXLinkageKind = Integer;

const
  CXLinkage_Invalid = 0;
  CXLinkage_NoLinkage = CXLinkage_Invalid + 1;
  CXLinkage_Internal = CXLinkage_NoLinkage + 1;
  CXLinkage_UniqueExternal = CXLinkage_Internal + 1;
  CXLinkage_External = CXLinkage_UniqueExternal + 1;

(**
 * Determine the linkage of the entity referred to by a given cursor.
 *)
function clang_getCursorLinkage(cursor: TCXCursor): TCXLinkageKind; cdecl external LIBCLANG;

type
  TCXVisibilityKind = Integer;

const
  CXVisibility_Invalid = 0;
  CXVisibility_Hidden = CXVisibility_Invalid + 1;
  CXVisibility_Protected = CXVisibility_Hidden + 1;
  CXVisibility_Default = CXVisibility_Protected + 1;

(**
 * Describe the visibility of the entity referred to by a cursor.
 *
 * This returns the default visibility if not explicitly specified by
 * a visibility attribute. The default visibility may be changed by
 * commandline arguments.
 *
 * \param cursor The cursor to query.
 *
 * \returns The visibility of the cursor.
 *)
function clang_getCursorVisibility(cursor: TCXCursor): TCXVisibilityKind; cdecl external LIBCLANG;

(**
 * Determine the availability of the entity that this cursor refers to,
 * taking the current target platform into account.
 *
 * \param cursor The cursor to query.
 *
 * \returns The availability of the cursor.
 *)
function clang_getCursorAvailability(cursor: TCXCursor): TCXAvailabilityKind; cdecl external LIBCLANG;

(**
 * Describes the availability of a given entity on a particular platform, e.g.,
 * a particular class might only be available on Mac OS 10.7 or newer.
 *)
type
  TCXPlatformAvailability = record
    Platform: TCXString;
    Introduced: TCXVersion;
    Deprecated: TCXVersion;
    Obsoleted: TCXVersion;
    Unavailable: Integer;
    Message: TCXString;
  end;
  PCXPlatformAvailability = ^TCXPlatformAvailability;

(**
 * Determine the availability of the entity that this cursor refers to
 * on any platforms for which availability information is known.
 *
 * \param cursor The cursor to query.
 *
 * \param always_deprecated If non-NULL, will be set to indicate whether the
 * entity is deprecated on all platforms.
 *
 * \param deprecated_message If non-NULL, will be set to the message text
 * provided along with the unconditional deprecation of this entity. The client
 * is responsible for deallocating this string.
 *
 * \param always_unavailable If non-NULL, will be set to indicate whether the
 * entity is unavailable on all platforms.
 *
 * \param unavailable_message If non-NULL, will be set to the message text
 * provided along with the unconditional unavailability of this entity. The
 * client is responsible for deallocating this string.
 *
 * \param availability If non-NULL, an array of CXPlatformAvailability instances
 * that will be populated with platform availability information, up to either
 * the number of platforms for which availability information is available (as
 * returned by this function) or \c availability_size, whichever is smaller.
 *
 * \param availability_size The number of elements available in the
 * \c availability array.
 *
 * \returns The number of platforms (N) for which availability information is
 * available (which is unrelated to \c availability_size).
 *
 * Note that the client is responsible for calling
 * \c clang_disposeCXPlatformAvailability to free each of the
 * platform-availability structures returned. There are
 * \c min(N, availability_size) such structures.
 *)
function clang_getCursorPlatformAvailability(cursor: TCXCursor; always_deprecated: PInteger; deprecated_message: PCXString; always_unavailable: PInteger; unavailable_message: PCXString; availability: PCXPlatformAvailability; availability_size: Integer): Integer; cdecl external LIBCLANG;

(**
 * Free the memory associated with a \c CXPlatformAvailability structure.
 *)
procedure clang_disposeCXPlatformAvailability(availability: PCXPlatformAvailability); cdecl external LIBCLANG;

(**
 * Describe the "language" of the entity referred to by a cursor.
 *)
type
  TCXLanguageKind = Integer;

const
  CXLanguage_Invalid = 0;
  CXLanguage_C = CXLanguage_Invalid + 1;
  CXLanguage_ObjC = CXLanguage_C + 1;
  CXLanguage_CPlusPlus = CXLanguage_ObjC + 1;

(**
 * Determine the "language" of the entity referred to by a given cursor.
 *)
function clang_getCursorLanguage(cursor: TCXCursor): TCXLanguageKind; cdecl external LIBCLANG;

(**
 * Describe the "thread-local storage (TLS) kind" of the declaration
 * referred to by a cursor.
 *)
type
  TCXTLSKind = Integer;

const
  CXTLS_None = 0;
  CXTLS_Dynamic = CXTLS_None + 1;
  CXTLS_Static = CXTLS_Dynamic + 1;

(**
 * Determine the "thread-local storage (TLS) kind" of the declaration
 * referred to by a cursor.
 *)
function clang_getCursorTLSKind(cursor: TCXCursor): TCXTLSKind; cdecl external LIBCLANG;

(**
 * Returns the translation unit that a cursor originated from.
 *)
function clang_Cursor_getTranslationUnit(p1: TCXCursor): TCXTranslationUnit; cdecl external LIBCLANG;

(**
 * A fast container representing a set of CXCursors.
 *)
type TCXCursorSet = Pointer;

(**
 * Creates an empty CXCursorSet.
 *)
function clang_createCXCursorSet(): TCXCursorSet; cdecl external LIBCLANG;

(**
 * Disposes a CXCursorSet and releases its associated memory.
 *)
procedure clang_disposeCXCursorSet(cset: TCXCursorSet); cdecl external LIBCLANG;

(**
 * Queries a CXCursorSet to see if it contains a specific CXCursor.
 *
 * \returns non-zero if the set contains the specified cursor.
*)
function clang_CXCursorSet_contains(cset: TCXCursorSet; cursor: TCXCursor): Cardinal; cdecl external LIBCLANG;

(**
 * Inserts a CXCursor into a CXCursorSet.
 *
 * \returns zero if the CXCursor was already in the set, and non-zero otherwise.
*)
function clang_CXCursorSet_insert(cset: TCXCursorSet; cursor: TCXCursor): Cardinal; cdecl external LIBCLANG;

(**
 * Determine the semantic parent of the given cursor.
 *
 * The semantic parent of a cursor is the cursor that semantically contains
 * the given \p cursor. For many declarations, the lexical and semantic parents
 * are equivalent (the lexical parent is returned by
 * \c clang_getCursorLexicalParent()). They diverge when declarations or
 * definitions are provided out-of-line. For example:
 *
 * \code
 * class C {
 *  void f();
 * };
 *
 * void C::f() { }
 * \endcode
 *
 * In the out-of-line definition of \c C::f, the semantic parent is
 * the class \c C, of which this function is a member. The lexical parent is
 * the place where the declaration actually occurs in the source code; in this
 * case, the definition occurs in the translation unit. In general, the
 * lexical parent for a given entity can change without affecting the semantics
 * of the program, and the lexical parent of different declarations of the
 * same entity may be different. Changing the semantic parent of a declaration,
 * on the other hand, can have a major impact on semantics, and redeclarations
 * of a particular entity should all have the same semantic context.
 *
 * In the example above, both declarations of \c C::f have \c C as their
 * semantic context, while the lexical context of the first \c C::f is \c C
 * and the lexical context of the second \c C::f is the translation unit.
 *
 * For global declarations, the semantic parent is the translation unit.
 *)
function clang_getCursorSemanticParent(cursor: TCXCursor): TCXCursor; cdecl external LIBCLANG;

(**
 * Determine the lexical parent of the given cursor.
 *
 * The lexical parent of a cursor is the cursor in which the given \p cursor
 * was actually written. For many declarations, the lexical and semantic parents
 * are equivalent (the semantic parent is returned by
 * \c clang_getCursorSemanticParent()). They diverge when declarations or
 * definitions are provided out-of-line. For example:
 *
 * \code
 * class C {
 *  void f();
 * };
 *
 * void C::f() { }
 * \endcode
 *
 * In the out-of-line definition of \c C::f, the semantic parent is
 * the class \c C, of which this function is a member. The lexical parent is
 * the place where the declaration actually occurs in the source code; in this
 * case, the definition occurs in the translation unit. In general, the
 * lexical parent for a given entity can change without affecting the semantics
 * of the program, and the lexical parent of different declarations of the
 * same entity may be different. Changing the semantic parent of a declaration,
 * on the other hand, can have a major impact on semantics, and redeclarations
 * of a particular entity should all have the same semantic context.
 *
 * In the example above, both declarations of \c C::f have \c C as their
 * semantic context, while the lexical context of the first \c C::f is \c C
 * and the lexical context of the second \c C::f is the translation unit.
 *
 * For declarations written in the global scope, the lexical parent is
 * the translation unit.
 *)
function clang_getCursorLexicalParent(cursor: TCXCursor): TCXCursor; cdecl external LIBCLANG;

(**
 * Determine the set of methods that are overridden by the given
 * method.
 *
 * In both Objective-C and C++, a method (aka virtual member function,
 * in C++) can override a virtual method in a base class. For
 * Objective-C, a method is said to override any method in the class's
 * base class, its protocols, or its categories' protocols, that has the same
 * selector and is of the same kind (class or instance).
 * If no such method exists, the search continues to the class's superclass,
 * its protocols, and its categories, and so on. A method from an Objective-C
 * implementation is considered to override the same methods as its
 * corresponding method in the interface.
 *
 * For C++, a virtual member function overrides any virtual member
 * function with the same signature that occurs in its base
 * classes. With multiple inheritance, a virtual member function can
 * override several virtual member functions coming from different
 * base classes.
 *
 * In all cases, this function determines the immediate overridden
 * method, rather than all of the overridden methods. For example, if
 * a method is originally declared in a class A, then overridden in B
 * (which in inherits from A) and also in C (which inherited from B),
 * then the only overridden method returned from this function when
 * invoked on C's method will be B's method. The client may then
 * invoke this function again, given the previously-found overridden
 * methods, to map out the complete method-override set.
 *
 * \param cursor A cursor representing an Objective-C or C++
 * method. This routine will compute the set of methods that this
 * method overrides.
 *
 * \param overridden A pointer whose pointee will be replaced with a
 * pointer to an array of cursors, representing the set of overridden
 * methods. If there are no overridden methods, the pointee will be
 * set to NULL. The pointee must be freed via a call to
 * \c clang_disposeOverriddenCursors().
 *
 * \param num_overridden A pointer to the number of overridden
 * functions, will be set to the number of overridden functions in the
 * array pointed to by \p overridden.
 *)
procedure clang_getOverriddenCursors(cursor: TCXCursor; overridden: PPCXCursor; num_overridden: PCardinal); cdecl external LIBCLANG;

(**
 * Free the set of overridden cursors returned by \c
 * clang_getOverriddenCursors().
 *)
procedure clang_disposeOverriddenCursors(overridden: PCXCursor); cdecl external LIBCLANG;

(**
 * Retrieve the file that is included by the given inclusion directive
 * cursor.
 *)
function clang_getIncludedFile(cursor: TCXCursor): TCXFile; cdecl external LIBCLANG;

(**
 * @}
 *)

(**
 * \defgroup CINDEX_CURSOR_SOURCE Mapping between cursors and source code
 *
 * Cursors represent a location within the Abstract Syntax Tree (AST). These
 * routines help map between cursors and the physical locations where the
 * described entities occur in the source code. The mapping is provided in
 * both directions, so one can map from source code to the AST and back.
 *
 * @{
 *)

(**
 * Map a source location to the cursor that describes the entity at that
 * location in the source code.
 *
 * clang_getCursor() maps an arbitrary source location within a translation
 * unit down to the most specific cursor that describes the entity at that
 * location. For example, given an expression \c x + y, invoking
 * clang_getCursor() with a source location pointing to "x" will return the
 * cursor for "x"; similarly for "y". If the cursor points anywhere between
 * "x" or "y" (e.g., on the + or the whitespace around it), clang_getCursor()
 * will return a cursor referring to the "+" expression.
 *
 * \returns a cursor representing the entity at the given source location, or
 * a NULL cursor if no such entity can be found.
 *)
function clang_getCursor(p1: TCXTranslationUnit; p2: TCXSourceLocation): TCXCursor; cdecl external LIBCLANG;

(**
 * Retrieve the physical location of the source constructor referenced
 * by the given cursor.
 *
 * The location of a declaration is typically the location of the name of that
 * declaration, where the name of that declaration would occur if it is
 * unnamed, or some keyword that introduces that particular declaration.
 * The location of a reference is where that reference occurs within the
 * source code.
 *)
function clang_getCursorLocation(p1: TCXCursor): TCXSourceLocation; cdecl external LIBCLANG;

(**
 * Retrieve the physical extent of the source construct referenced by
 * the given cursor.
 *
 * The extent of a cursor starts with the file/line/column pointing at the
 * first character within the source construct that the cursor refers to and
 * ends with the last character within that source construct. For a
 * declaration, the extent covers the declaration itself. For a reference,
 * the extent covers the location of the reference (e.g., where the referenced
 * entity was actually used).
 *)
function clang_getCursorExtent(p1: TCXCursor): TCXSourceRange; cdecl external LIBCLANG;

(**
 * @}
 *)

(**
 * \defgroup CINDEX_TYPES Type information for CXCursors
 *
 * @{
 *)

(**
 * Describes the kind of type
 *)
type
  TCXTypeKind = Integer;

const
  CXType_Invalid = 0;
  CXType_Unexposed = 1;
  CXType_Void = 2;
  CXType_Bool = 3;
  CXType_Char_U = 4;
  CXType_UChar = 5;
  CXType_Char16 = 6;
  CXType_Char32 = 7;
  CXType_UShort = 8;
  CXType_UInt = 9;
  CXType_ULong = 10;
  CXType_ULongLong = 11;
  CXType_UInt128 = 12;
  CXType_Char_S = 13;
  CXType_SChar = 14;
  CXType_WChar = 15;
  CXType_Short = 16;
  CXType_Int = 17;
  CXType_Long = 18;
  CXType_LongLong = 19;
  CXType_Int128 = 20;
  CXType_Float = 21;
  CXType_Double = 22;
  CXType_LongDouble = 23;
  CXType_NullPtr = 24;
  CXType_Overload = 25;
  CXType_Dependent = 26;
  CXType_ObjCId = 27;
  CXType_ObjCClass = 28;
  CXType_ObjCSel = 29;
  CXType_Float128 = 30;
  CXType_Half = 31;
  CXType_Float16 = 32;
  CXType_ShortAccum = 33;
  CXType_Accum = 34;
  CXType_LongAccum = 35;
  CXType_UShortAccum = 36;
  CXType_UAccum = 37;
  CXType_ULongAccum = 38;
  CXType_FirstBuiltin = CXType_Void;
  CXType_LastBuiltin = CXType_ULongAccum;
  CXType_Complex = 100;
  CXType_Pointer = 101;
  CXType_BlockPointer = 102;
  CXType_LValueReference = 103;
  CXType_RValueReference = 104;
  CXType_Record = 105;
  CXType_Enum = 106;
  CXType_Typedef = 107;
  CXType_ObjCInterface = 108;
  CXType_ObjCObjectPointer = 109;
  CXType_FunctionNoProto = 110;
  CXType_FunctionProto = 111;
  CXType_ConstantArray = 112;
  CXType_Vector = 113;
  CXType_IncompleteArray = 114;
  CXType_VariableArray = 115;
  CXType_DependentSizedArray = 116;
  CXType_MemberPointer = 117;
  CXType_Auto = 118;
  CXType_Elaborated = 119;
  CXType_Pipe = 120;
  CXType_OCLImage1dRO = 121;
  CXType_OCLImage1dArrayRO = 122;
  CXType_OCLImage1dBufferRO = 123;
  CXType_OCLImage2dRO = 124;
  CXType_OCLImage2dArrayRO = 125;
  CXType_OCLImage2dDepthRO = 126;
  CXType_OCLImage2dArrayDepthRO = 127;
  CXType_OCLImage2dMSAARO = 128;
  CXType_OCLImage2dArrayMSAARO = 129;
  CXType_OCLImage2dMSAADepthRO = 130;
  CXType_OCLImage2dArrayMSAADepthRO = 131;
  CXType_OCLImage3dRO = 132;
  CXType_OCLImage1dWO = 133;
  CXType_OCLImage1dArrayWO = 134;
  CXType_OCLImage1dBufferWO = 135;
  CXType_OCLImage2dWO = 136;
  CXType_OCLImage2dArrayWO = 137;
  CXType_OCLImage2dDepthWO = 138;
  CXType_OCLImage2dArrayDepthWO = 139;
  CXType_OCLImage2dMSAAWO = 140;
  CXType_OCLImage2dArrayMSAAWO = 141;
  CXType_OCLImage2dMSAADepthWO = 142;
  CXType_OCLImage2dArrayMSAADepthWO = 143;
  CXType_OCLImage3dWO = 144;
  CXType_OCLImage1dRW = 145;
  CXType_OCLImage1dArrayRW = 146;
  CXType_OCLImage1dBufferRW = 147;
  CXType_OCLImage2dRW = 148;
  CXType_OCLImage2dArrayRW = 149;
  CXType_OCLImage2dDepthRW = 150;
  CXType_OCLImage2dArrayDepthRW = 151;
  CXType_OCLImage2dMSAARW = 152;
  CXType_OCLImage2dArrayMSAARW = 153;
  CXType_OCLImage2dMSAADepthRW = 154;
  CXType_OCLImage2dArrayMSAADepthRW = 155;
  CXType_OCLImage3dRW = 156;
  CXType_OCLSampler = 157;
  CXType_OCLEvent = 158;
  CXType_OCLQueue = 159;
  CXType_OCLReserveID = 160;
  CXType_ObjCObject = 161;
  CXType_ObjCTypeParam = 162;
  CXType_Attributed = 163;
  CXType_OCLIntelSubgroupAVCMcePayload = 164;
  CXType_OCLIntelSubgroupAVCImePayload = 165;
  CXType_OCLIntelSubgroupAVCRefPayload = 166;
  CXType_OCLIntelSubgroupAVCSicPayload = 167;
  CXType_OCLIntelSubgroupAVCMceResult = 168;
  CXType_OCLIntelSubgroupAVCImeResult = 169;
  CXType_OCLIntelSubgroupAVCRefResult = 170;
  CXType_OCLIntelSubgroupAVCSicResult = 171;
  CXType_OCLIntelSubgroupAVCImeResultSingleRefStreamout = 172;
  CXType_OCLIntelSubgroupAVCImeResultDualRefStreamout = 173;
  CXType_OCLIntelSubgroupAVCImeSingleRefStreamin = 174;
  CXType_OCLIntelSubgroupAVCImeDualRefStreamin = 175;
  CXType_ExtVector = 176;

(**
 * Describes the calling convention of a function type
 *)
type
  TCXCallingConv = Integer;

const
  CXCallingConv_Default = 0;
  CXCallingConv_C = 1;
  CXCallingConv_X86StdCall = 2;
  CXCallingConv_X86FastCall = 3;
  CXCallingConv_X86ThisCall = 4;
  CXCallingConv_X86Pascal = 5;
  CXCallingConv_AAPCS = 6;
  CXCallingConv_AAPCS_VFP = 7;
  CXCallingConv_X86RegCall = 8;
  CXCallingConv_IntelOclBicc = 9;
  CXCallingConv_Win64 = 10;
  CXCallingConv_X86_64Win64 = CXCallingConv_Win64;
  CXCallingConv_X86_64SysV = 11;
  CXCallingConv_X86VectorCall = 12;
  CXCallingConv_Swift = 13;
  CXCallingConv_PreserveMost = 14;
  CXCallingConv_PreserveAll = 15;
  CXCallingConv_AArch64VectorCall = 16;
  CXCallingConv_Invalid = 100;
  CXCallingConv_Unexposed = 200;

(**
 * The type of an element in the abstract syntax tree.
 *
 *)
type
  TCXType = record
    kind: TCXTypeKind;
    data: array [0..2-1] of Pointer;
  end;
  PCXType = ^TCXType;

(**
 * Retrieve the type of a CXCursor (if any).
 *)
function clang_getCursorType(C: TCXCursor): TCXType; cdecl external LIBCLANG;

(**
 * Pretty-print the underlying type using the rules of the
 * language of the translation unit from which it came.
 *
 * If the type is invalid, an empty string is returned.
 *)
function clang_getTypeSpelling(CT: TCXType): TCXString; cdecl external LIBCLANG;

(**
 * Retrieve the underlying type of a typedef declaration.
 *
 * If the cursor does not reference a typedef declaration, an invalid type is
 * returned.
 *)
function clang_getTypedefDeclUnderlyingType(C: TCXCursor): TCXType; cdecl external LIBCLANG;

(**
 * Retrieve the integer type of an enum declaration.
 *
 * If the cursor does not reference an enum declaration, an invalid type is
 * returned.
 *)
function clang_getEnumDeclIntegerType(C: TCXCursor): TCXType; cdecl external LIBCLANG;

(**
 * Retrieve the integer value of an enum constant declaration as a signed
 *  long long.
 *
 * If the cursor does not reference an enum constant declaration, LLONG_MIN is returned.
 * Since this is also potentially a valid constant value, the kind of the cursor
 * must be verified before calling this function.
 *)
function clang_getEnumConstantDeclValue(C: TCXCursor): Int64; cdecl external LIBCLANG;

(**
 * Retrieve the integer value of an enum constant declaration as an unsigned
 *  long long.
 *
 * If the cursor does not reference an enum constant declaration, ULLONG_MAX is returned.
 * Since this is also potentially a valid constant value, the kind of the cursor
 * must be verified before calling this function.
 *)
function clang_getEnumConstantDeclUnsignedValue(C: TCXCursor): UInt64; cdecl external LIBCLANG;

(**
 * Retrieve the bit width of a bit field declaration as an integer.
 *
 * If a cursor that is not a bit field declaration is passed in, -1 is returned.
 *)
function clang_getFieldDeclBitWidth(C: TCXCursor): Integer; cdecl external LIBCLANG;

(**
 * Retrieve the number of non-variadic arguments associated with a given
 * cursor.
 *
 * The number of arguments can be determined for calls as well as for
 * declarations of functions or methods. For other cursors -1 is returned.
 *)
function clang_Cursor_getNumArguments(C: TCXCursor): Integer; cdecl external LIBCLANG;

(**
 * Retrieve the argument cursor of a function or method.
 *
 * The argument cursor can be determined for calls as well as for declarations
 * of functions or methods. For other cursors and for invalid indices, an
 * invalid cursor is returned.
 *)
function clang_Cursor_getArgument(C: TCXCursor; i: Cardinal): TCXCursor; cdecl external LIBCLANG;

(**
 * Describes the kind of a template argument.
 *
 * See the definition of llvm::clang::TemplateArgument::ArgKind for full
 * element descriptions.
 *)
type
  TCXTemplateArgumentKind = Integer;

const
  CXTemplateArgumentKind_Null = 0;
  CXTemplateArgumentKind_Type = CXTemplateArgumentKind_Null + 1;
  CXTemplateArgumentKind_Declaration = CXTemplateArgumentKind_Type + 1;
  CXTemplateArgumentKind_NullPtr = CXTemplateArgumentKind_Declaration + 1;
  CXTemplateArgumentKind_Integral = CXTemplateArgumentKind_NullPtr + 1;
  CXTemplateArgumentKind_Template = CXTemplateArgumentKind_Integral + 1;
  CXTemplateArgumentKind_TemplateExpansion = CXTemplateArgumentKind_Template + 1;
  CXTemplateArgumentKind_Expression = CXTemplateArgumentKind_TemplateExpansion + 1;
  CXTemplateArgumentKind_Pack = CXTemplateArgumentKind_Expression + 1;
  CXTemplateArgumentKind_Invalid = CXTemplateArgumentKind_Pack + 1;

(**
 *Returns the number of template args of a function decl representing a
 * template specialization.
 *
 * If the argument cursor cannot be converted into a template function
 * declaration, -1 is returned.
 *
 * For example, for the following declaration and specialization:
 *   template <typename T, int kInt, bool kBool>
 *   void foo() { ... }
 *
 *   template <>
 *   void foo<float, -7, true>();
 *
 * The value 3 would be returned from this call.
 *)
function clang_Cursor_getNumTemplateArguments(C: TCXCursor): Integer; cdecl external LIBCLANG;

(**
 * Retrieve the kind of the I'th template argument of the CXCursor C.
 *
 * If the argument CXCursor does not represent a FunctionDecl, an invalid
 * template argument kind is returned.
 *
 * For example, for the following declaration and specialization:
 *   template <typename T, int kInt, bool kBool>
 *   void foo() { ... }
 *
 *   template <>
 *   void foo<float, -7, true>();
 *
 * For I = 0, 1, and 2, Type, Integral, and Integral will be returned,
 * respectively.
 *)
function clang_Cursor_getTemplateArgumentKind(C: TCXCursor; I: Cardinal): TCXTemplateArgumentKind; cdecl external LIBCLANG;

(**
 * Retrieve a CXType representing the type of a TemplateArgument of a
 *  function decl representing a template specialization.
 *
 * If the argument CXCursor does not represent a FunctionDecl whose I'th
 * template argument has a kind of CXTemplateArgKind_Integral, an invalid type
 * is returned.
 *
 * For example, for the following declaration and specialization:
 *   template <typename T, int kInt, bool kBool>
 *   void foo() { ... }
 *
 *   template <>
 *   void foo<float, -7, true>();
 *
 * If called with I = 0, "float", will be returned.
 * Invalid types will be returned for I == 1 or 2.
 *)
function clang_Cursor_getTemplateArgumentType(C: TCXCursor; I: Cardinal): TCXType; cdecl external LIBCLANG;

(**
 * Retrieve the value of an Integral TemplateArgument (of a function
 *  decl representing a template specialization) as a signed long long.
 *
 * It is undefined to call this function on a CXCursor that does not represent a
 * FunctionDecl or whose I'th template argument is not an integral value.
 *
 * For example, for the following declaration and specialization:
 *   template <typename T, int kInt, bool kBool>
 *   void foo() { ... }
 *
 *   template <>
 *   void foo<float, -7, true>();
 *
 * If called with I = 1 or 2, -7 or true will be returned, respectively.
 * For I == 0, this function's behavior is undefined.
 *)
function clang_Cursor_getTemplateArgumentValue(C: TCXCursor; I: Cardinal): Int64; cdecl external LIBCLANG;

(**
 * Retrieve the value of an Integral TemplateArgument (of a function
 *  decl representing a template specialization) as an unsigned long long.
 *
 * It is undefined to call this function on a CXCursor that does not represent a
 * FunctionDecl or whose I'th template argument is not an integral value.
 *
 * For example, for the following declaration and specialization:
 *   template <typename T, int kInt, bool kBool>
 *   void foo() { ... }
 *
 *   template <>
 *   void foo<float, 2147483649, true>();
 *
 * If called with I = 1 or 2, 2147483649 or true will be returned, respectively.
 * For I == 0, this function's behavior is undefined.
 *)
function clang_Cursor_getTemplateArgumentUnsignedValue(C: TCXCursor; I: Cardinal): UInt64; cdecl external LIBCLANG;

(**
 * Determine whether two CXTypes represent the same type.
 *
 * \returns non-zero if the CXTypes represent the same type and
 *          zero otherwise.
 *)
function clang_equalTypes(A: TCXType; B: TCXType): Cardinal; cdecl external LIBCLANG;

(**
 * Return the canonical type for a CXType.
 *
 * Clang's type system explicitly models typedefs and all the ways
 * a specific type can be represented.  The canonical type is the underlying
 * type with all the "sugar" removed.  For example, if 'T' is a typedef
 * for 'int', the canonical type for 'T' would be 'int'.
 *)
function clang_getCanonicalType(T: TCXType): TCXType; cdecl external LIBCLANG;

(**
 * Determine whether a CXType has the "const" qualifier set,
 * without looking through typedefs that may have added "const" at a
 * different level.
 *)
function clang_isConstQualifiedType(T: TCXType): Cardinal; cdecl external LIBCLANG;

(**
 * Determine whether a  CXCursor that is a macro, is
 * function like.
 *)
function clang_Cursor_isMacroFunctionLike(C: TCXCursor): Cardinal; cdecl external LIBCLANG;

(**
 * Determine whether a  CXCursor that is a macro, is a
 * builtin one.
 *)
function clang_Cursor_isMacroBuiltin(C: TCXCursor): Cardinal; cdecl external LIBCLANG;

(**
 * Determine whether a  CXCursor that is a function declaration, is an
 * inline declaration.
 *)
function clang_Cursor_isFunctionInlined(C: TCXCursor): Cardinal; cdecl external LIBCLANG;

(**
 * Determine whether a CXType has the "volatile" qualifier set,
 * without looking through typedefs that may have added "volatile" at
 * a different level.
 *)
function clang_isVolatileQualifiedType(T: TCXType): Cardinal; cdecl external LIBCLANG;

(**
 * Determine whether a CXType has the "restrict" qualifier set,
 * without looking through typedefs that may have added "restrict" at a
 * different level.
 *)
function clang_isRestrictQualifiedType(T: TCXType): Cardinal; cdecl external LIBCLANG;

(**
 * Returns the address space of the given type.
 *)
function clang_getAddressSpace(T: TCXType): Cardinal; cdecl external LIBCLANG;

(**
 * Returns the typedef name of the given type.
 *)
function clang_getTypedefName(CT: TCXType): TCXString; cdecl external LIBCLANG;

(**
 * For pointer types, returns the type of the pointee.
 *)
function clang_getPointeeType(T: TCXType): TCXType; cdecl external LIBCLANG;

(**
 * Return the cursor for the declaration of the given type.
 *)
function clang_getTypeDeclaration(T: TCXType): TCXCursor; cdecl external LIBCLANG;

(**
 * Returns the Objective-C type encoding for the specified declaration.
 *)
function clang_getDeclObjCTypeEncoding(C: TCXCursor): TCXString; cdecl external LIBCLANG;

(**
 * Returns the Objective-C type encoding for the specified CXType.
 *)
function clang_Type_getObjCEncoding(_type: TCXType): TCXString; cdecl external LIBCLANG;

(**
 * Retrieve the spelling of a given CXTypeKind.
 *)
function clang_getTypeKindSpelling(K: TCXTypeKind): TCXString; cdecl external LIBCLANG;

(**
 * Retrieve the calling convention associated with a function type.
 *
 * If a non-function type is passed in, CXCallingConv_Invalid is returned.
 *)
function clang_getFunctionTypeCallingConv(T: TCXType): TCXCallingConv; cdecl external LIBCLANG;

(**
 * Retrieve the return type associated with a function type.
 *
 * If a non-function type is passed in, an invalid type is returned.
 *)
function clang_getResultType(T: TCXType): TCXType; cdecl external LIBCLANG;

(**
 * Retrieve the exception specification type associated with a function type.
 * This is a value of type CXCursor_ExceptionSpecificationKind.
 * If a non-function type is passed in, an error code of -1 is returned.
 *)
function clang_getExceptionSpecificationType(T: TCXType): Integer; cdecl external LIBCLANG;

(**
 * Retrieve the number of non-variadic parameters associated with a
 * function type.
 *
 * If a non-function type is passed in, -1 is returned.
 *)
function clang_getNumArgTypes(T: TCXType): Integer; cdecl external LIBCLANG;

(**
 * Retrieve the type of a parameter of a function type.
 *
 * If a non-function type is passed in or the function does not have enough
 * parameters, an invalid type is returned.
 *)
function clang_getArgType(T: TCXType; i: Cardinal): TCXType; cdecl external LIBCLANG;

(**
 * Retrieves the base type of the ObjCObjectType.
 *
 * If the type is not an ObjC object, an invalid type is returned.
 *)
function clang_Type_getObjCObjectBaseType(T: TCXType): TCXType; cdecl external LIBCLANG;

(**
 * Retrieve the number of protocol references associated with an ObjC object/id.
 *
 * If the type is not an ObjC object, 0 is returned.
 *)
function clang_Type_getNumObjCProtocolRefs(T: TCXType): Cardinal; cdecl external LIBCLANG;

(**
 * Retrieve the decl for a protocol reference for an ObjC object/id.
 *
 * If the type is not an ObjC object or there are not enough protocol
 * references, an invalid cursor is returned.
 *)
function clang_Type_getObjCProtocolDecl(T: TCXType; i: Cardinal): TCXCursor; cdecl external LIBCLANG;

(**
 * Retreive the number of type arguments associated with an ObjC object.
 *
 * If the type is not an ObjC object, 0 is returned.
 *)
function clang_Type_getNumObjCTypeArgs(T: TCXType): Cardinal; cdecl external LIBCLANG;

(**
 * Retrieve a type argument associated with an ObjC object.
 *
 * If the type is not an ObjC or the index is not valid,
 * an invalid type is returned.
 *)
function clang_Type_getObjCTypeArg(T: TCXType; i: Cardinal): TCXType; cdecl external LIBCLANG;

(**
 * Return 1 if the CXType is a variadic function type, and 0 otherwise.
 *)
function clang_isFunctionTypeVariadic(T: TCXType): Cardinal; cdecl external LIBCLANG;

(**
 * Retrieve the return type associated with a given cursor.
 *
 * This only returns a valid type if the cursor refers to a function or method.
 *)
function clang_getCursorResultType(C: TCXCursor): TCXType; cdecl external LIBCLANG;

(**
 * Retrieve the exception specification type associated with a given cursor.
 * This is a value of type CXCursor_ExceptionSpecificationKind.
 *
 * This only returns a valid result if the cursor refers to a function or method.
 *)
function clang_getCursorExceptionSpecificationType(C: TCXCursor): Integer; cdecl external LIBCLANG;

(**
 * Return 1 if the CXType is a POD (plain old data) type, and 0
 *  otherwise.
 *)
function clang_isPODType(T: TCXType): Cardinal; cdecl external LIBCLANG;

(**
 * Return the element type of an array, complex, or vector type.
 *
 * If a type is passed in that is not an array, complex, or vector type,
 * an invalid type is returned.
 *)
function clang_getElementType(T: TCXType): TCXType; cdecl external LIBCLANG;

(**
 * Return the number of elements of an array or vector type.
 *
 * If a type is passed in that is not an array or vector type,
 * -1 is returned.
 *)
function clang_getNumElements(T: TCXType): Int64; cdecl external LIBCLANG;

(**
 * Return the element type of an array type.
 *
 * If a non-array type is passed in, an invalid type is returned.
 *)
function clang_getArrayElementType(T: TCXType): TCXType; cdecl external LIBCLANG;

(**
 * Return the array size of a constant array.
 *
 * If a non-array type is passed in, -1 is returned.
 *)
function clang_getArraySize(T: TCXType): Int64; cdecl external LIBCLANG;

(**
 * Retrieve the type named by the qualified-id.
 *
 * If a non-elaborated type is passed in, an invalid type is returned.
 *)
function clang_Type_getNamedType(T: TCXType): TCXType; cdecl external LIBCLANG;

(**
 * Determine if a typedef is 'transparent' tag.
 *
 * A typedef is considered 'transparent' if it shares a name and spelling
 * location with its underlying tag type, as is the case with the NS_ENUM macro.
 *
 * \returns non-zero if transparent and zero otherwise.
 *)
function clang_Type_isTransparentTagTypedef(T: TCXType): Cardinal; cdecl external LIBCLANG;

type
  TCXTypeNullabilityKind = Integer;

const
  CXTypeNullability_NonNull = 0;
  CXTypeNullability_Nullable = 1;
  CXTypeNullability_Unspecified = 2;
  CXTypeNullability_Invalid = 3;

(**
 * Retrieve the nullability kind of a pointer type.
 *)
function clang_Type_getNullability(T: TCXType): TCXTypeNullabilityKind; cdecl external LIBCLANG;

(**
 * List the possible error codes for \c clang_Type_getSizeOf,
 *   \c clang_Type_getAlignOf, \c clang_Type_getOffsetOf and
 *   \c clang_Cursor_getOffsetOf.
 *
 * A value of this enumeration type can be returned if the target type is not
 * a valid argument to sizeof, alignof or offsetof.
 *)
type
  TCXTypeLayoutError = Integer;

const
  CXTypeLayoutError_Invalid = -1;
  CXTypeLayoutError_Incomplete = -2;
  CXTypeLayoutError_Dependent = -3;
  CXTypeLayoutError_NotConstantSize = -4;
  CXTypeLayoutError_InvalidFieldName = -5;
  CXTypeLayoutError_Undeduced = -6;

(**
 * Return the alignment of a type in bytes as per C++[expr.alignof]
 *   standard.
 *
 * If the type declaration is invalid, CXTypeLayoutError_Invalid is returned.
 * If the type declaration is an incomplete type, CXTypeLayoutError_Incomplete
 *   is returned.
 * If the type declaration is a dependent type, CXTypeLayoutError_Dependent is
 *   returned.
 * If the type declaration is not a constant size type,
 *   CXTypeLayoutError_NotConstantSize is returned.
 *)
function clang_Type_getAlignOf(T: TCXType): Int64; cdecl external LIBCLANG;

(**
 * Return the class type of an member pointer type.
 *
 * If a non-member-pointer type is passed in, an invalid type is returned.
 *)
function clang_Type_getClassType(T: TCXType): TCXType; cdecl external LIBCLANG;

(**
 * Return the size of a type in bytes as per C++[expr.sizeof] standard.
 *
 * If the type declaration is invalid, CXTypeLayoutError_Invalid is returned.
 * If the type declaration is an incomplete type, CXTypeLayoutError_Incomplete
 *   is returned.
 * If the type declaration is a dependent type, CXTypeLayoutError_Dependent is
 *   returned.
 *)
function clang_Type_getSizeOf(T: TCXType): Int64; cdecl external LIBCLANG;

(**
 * Return the offset of a field named S in a record of type T in bits
 *   as it would be returned by __offsetof__ as per C++11[18.2p4]
 *
 * If the cursor is not a record field declaration, CXTypeLayoutError_Invalid
 *   is returned.
 * If the field's type declaration is an incomplete type,
 *   CXTypeLayoutError_Incomplete is returned.
 * If the field's type declaration is a dependent type,
 *   CXTypeLayoutError_Dependent is returned.
 * If the field's name S is not found,
 *   CXTypeLayoutError_InvalidFieldName is returned.
 *)
function clang_Type_getOffsetOf(T: TCXType; const S: PAnsiChar): Int64; cdecl external LIBCLANG;

(**
 * Return the type that was modified by this attributed type.
 *
 * If the type is not an attributed type, an invalid type is returned.
 *)
function clang_Type_getModifiedType(T: TCXType): TCXType; cdecl external LIBCLANG;

(**
 * Return the offset of the field represented by the Cursor.
 *
 * If the cursor is not a field declaration, -1 is returned.
 * If the cursor semantic parent is not a record field declaration,
 *   CXTypeLayoutError_Invalid is returned.
 * If the field's type declaration is an incomplete type,
 *   CXTypeLayoutError_Incomplete is returned.
 * If the field's type declaration is a dependent type,
 *   CXTypeLayoutError_Dependent is returned.
 * If the field's name S is not found,
 *   CXTypeLayoutError_InvalidFieldName is returned.
 *)
function clang_Cursor_getOffsetOfField(C: TCXCursor): Int64; cdecl external LIBCLANG;

(**
 * Determine whether the given cursor represents an anonymous
 * tag or namespace.
 *)
function clang_Cursor_isAnonymous(C: TCXCursor): Cardinal; cdecl external LIBCLANG;

(**
 * Determine whether the given cursor represents an anonymous record
 * declaration.
 *)
function clang_Cursor_isAnonymousRecordDecl(C: TCXCursor): Cardinal; cdecl external LIBCLANG;

(**
 * Determine whether the given cursor represents an inline namespace
 * declaration.
 *)
function clang_Cursor_isInlineNamespace(C: TCXCursor): Cardinal; cdecl external LIBCLANG;

type
  TCXRefQualifierKind = Integer;

const
  CXRefQualifier_None = 0;
  CXRefQualifier_LValue = CXRefQualifier_None + 1;
  CXRefQualifier_RValue = CXRefQualifier_LValue + 1;

(**
 * Returns the number of template arguments for given template
 * specialization, or -1 if type \c T is not a template specialization.
 *)
function clang_Type_getNumTemplateArguments(T: TCXType): Integer; cdecl external LIBCLANG;

(**
 * Returns the type template argument of a template class specialization
 * at given index.
 *
 * This function only returns template type arguments and does not handle
 * template template arguments or variadic packs.
 *)
function clang_Type_getTemplateArgumentAsType(T: TCXType; i: Cardinal): TCXType; cdecl external LIBCLANG;

(**
 * Retrieve the ref-qualifier kind of a function or method.
 *
 * The ref-qualifier is returned for C++ functions or methods. For other types
 * or non-C++ declarations, CXRefQualifier_None is returned.
 *)
function clang_Type_getCXXRefQualifier(T: TCXType): TCXRefQualifierKind; cdecl external LIBCLANG;

(**
 * Returns non-zero if the cursor specifies a Record member that is a
 *   bitfield.
 *)
function clang_Cursor_isBitField(C: TCXCursor): Cardinal; cdecl external LIBCLANG;

(**
 * Returns 1 if the base class specified by the cursor with kind
 *   CX_CXXBaseSpecifier is virtual.
 *)
function clang_isVirtualBase(p1: TCXCursor): Cardinal; cdecl external LIBCLANG;

(**
 * Represents the C++ access control level to a base class for a
 * cursor with kind CX_CXXBaseSpecifier.
 *)
type
  TCX_CXXAccessSpecifier = Integer;

const
  CX_CXXInvalidAccessSpecifier = 0;
  CX_CXXPublic = CX_CXXInvalidAccessSpecifier + 1;
  CX_CXXProtected = CX_CXXPublic + 1;
  CX_CXXPrivate = CX_CXXProtected + 1;

(**
 * Returns the access control level for the referenced object.
 *
 * If the cursor refers to a C++ declaration, its access control level within its
 * parent scope is returned. Otherwise, if the cursor refers to a base specifier or
 * access specifier, the specifier itself is returned.
 *)
function clang_getCXXAccessSpecifier(p1: TCXCursor): TCX_CXXAccessSpecifier; cdecl external LIBCLANG;

(**
 * Represents the storage classes as declared in the source. CX_SC_Invalid
 * was added for the case that the passed cursor in not a declaration.
 *)
type
  TCX_StorageClass = Integer;

const
  CX_SC_Invalid = 0;
  CX_SC_None = CX_SC_Invalid + 1;
  CX_SC_Extern = CX_SC_None + 1;
  CX_SC_Static = CX_SC_Extern + 1;
  CX_SC_PrivateExtern = CX_SC_Static + 1;
  CX_SC_OpenCLWorkGroupLocal = CX_SC_PrivateExtern + 1;
  CX_SC_Auto = CX_SC_OpenCLWorkGroupLocal + 1;
  CX_SC_Register = CX_SC_Auto + 1;

(**
 * Returns the storage class for a function or variable declaration.
 *
 * If the passed in Cursor is not a function or variable declaration,
 * CX_SC_Invalid is returned else the storage class.
 *)
function clang_Cursor_getStorageClass(p1: TCXCursor): TCX_StorageClass; cdecl external LIBCLANG;

(**
 * Determine the number of overloaded declarations referenced by a
 * \c CXCursor_OverloadedDeclRef cursor.
 *
 * \param cursor The cursor whose overloaded declarations are being queried.
 *
 * \returns The number of overloaded declarations referenced by \c cursor. If it
 * is not a \c CXCursor_OverloadedDeclRef cursor, returns 0.
 *)
function clang_getNumOverloadedDecls(cursor: TCXCursor): Cardinal; cdecl external LIBCLANG;

(**
 * Retrieve a cursor for one of the overloaded declarations referenced
 * by a \c CXCursor_OverloadedDeclRef cursor.
 *
 * \param cursor The cursor whose overloaded declarations are being queried.
 *
 * \param index The zero-based index into the set of overloaded declarations in
 * the cursor.
 *
 * \returns A cursor representing the declaration referenced by the given
 * \c cursor at the specified \c index. If the cursor does not have an
 * associated set of overloaded declarations, or if the index is out of bounds,
 * returns \c clang_getNullCursor();
 *)
function clang_getOverloadedDecl(cursor: TCXCursor; index: Cardinal): TCXCursor; cdecl external LIBCLANG;

(**
 * @}
 *)

(**
 * \defgroup CINDEX_ATTRIBUTES Information for attributes
 *
 * @{
 *)

(**
 * For cursors representing an iboutletcollection attribute,
 *  this function returns the collection element type.
 *
 *)
function clang_getIBOutletCollectionType(p1: TCXCursor): TCXType; cdecl external LIBCLANG;

(**
 * @}
 *)

(**
 * \defgroup CINDEX_CURSOR_TRAVERSAL Traversing the AST with cursors
 *
 * These routines provide the ability to traverse the abstract syntax tree
 * using cursors.
 *
 * @{
 *)

(**
 * Describes how the traversal of the children of a particular
 * cursor should proceed after visiting a particular child cursor.
 *
 * A value of this enumeration type should be returned by each
 * \c CXCursorVisitor to indicate how clang_visitChildren() proceed.
 *)
type
  TCXChildVisitResult = Integer;

const
  CXChildVisit_Break = 0;
  CXChildVisit_Continue = CXChildVisit_Break + 1;
  CXChildVisit_Recurse = CXChildVisit_Continue + 1;

(**
 * Visitor invoked for each cursor found by a traversal.
 *
 * This visitor function will be invoked for each cursor found by
 * clang_visitCursorChildren(). Its first argument is the cursor being
 * visited, its second argument is the parent visitor for that cursor,
 * and its third argument is the client data provided to
 * clang_visitCursorChildren().
 *
 * The visitor should return one of the \c CXChildVisitResult values
 * to direct clang_visitCursorChildren().
 *)
type TCXCursorVisitor = function(cursor: TCXCursor; parent: TCXCursor; client_data: TCXClientData): TCXChildVisitResult; cdecl;

(**
 * Visit the children of a particular cursor.
 *
 * This function visits all the direct children of the given cursor,
 * invoking the given \p visitor function with the cursors of each
 * visited child. The traversal may be recursive, if the visitor returns
 * \c CXChildVisit_Recurse. The traversal may also be ended prematurely, if
 * the visitor returns \c CXChildVisit_Break.
 *
 * \param parent the cursor whose child may be visited. All kinds of
 * cursors can be visited, including invalid cursors (which, by
 * definition, have no children).
 *
 * \param visitor the visitor function that will be invoked for each
 * child of \p parent.
 *
 * \param client_data pointer data supplied by the client, which will
 * be passed to the visitor each time it is invoked.
 *
 * \returns a non-zero value if the traversal was terminated
 * prematurely by the visitor returning \c CXChildVisit_Break.
 *)
function clang_visitChildren(parent: TCXCursor; visitor: TCXCursorVisitor; client_data: TCXClientData): Cardinal; cdecl external LIBCLANG;

(**
 * @}
 *)

(**
 * \defgroup CINDEX_CURSOR_XREF Cross-referencing in the AST
 *
 * These routines provide the ability to determine references within and
 * across translation units, by providing the names of the entities referenced
 * by cursors, follow reference cursors to the declarations they reference,
 * and associate declarations with their definitions.
 *
 * @{
 *)

(**
 * Retrieve a Unified Symbol Resolution (USR) for the entity referenced
 * by the given cursor.
 *
 * A Unified Symbol Resolution (USR) is a string that identifies a particular
 * entity (function, class, variable, etc.) within a program. USRs can be
 * compared across translation units to determine, e.g., when references in
 * one translation refer to an entity defined in another translation unit.
 *)
function clang_getCursorUSR(p1: TCXCursor): TCXString; cdecl external LIBCLANG;

(**
 * Construct a USR for a specified Objective-C class.
 *)
function clang_constructUSR_ObjCClass(const class_name: PAnsiChar): TCXString; cdecl external LIBCLANG;

(**
 * Construct a USR for a specified Objective-C category.
 *)
function clang_constructUSR_ObjCCategory(const class_name: PAnsiChar; const category_name: PAnsiChar): TCXString; cdecl external LIBCLANG;

(**
 * Construct a USR for a specified Objective-C protocol.
 *)
function clang_constructUSR_ObjCProtocol(const protocol_name: PAnsiChar): TCXString; cdecl external LIBCLANG;

(**
 * Construct a USR for a specified Objective-C instance variable and
 *   the USR for its containing class.
 *)
function clang_constructUSR_ObjCIvar(const name: PAnsiChar; classUSR: TCXString): TCXString; cdecl external LIBCLANG;

(**
 * Construct a USR for a specified Objective-C method and
 *   the USR for its containing class.
 *)
function clang_constructUSR_ObjCMethod(const name: PAnsiChar; isInstanceMethod: Cardinal; classUSR: TCXString): TCXString; cdecl external LIBCLANG;

(**
 * Construct a USR for a specified Objective-C property and the USR
 *  for its containing class.
 *)
function clang_constructUSR_ObjCProperty(const _property: PAnsiChar; classUSR: TCXString): TCXString; cdecl external LIBCLANG;

(**
 * Retrieve a name for the entity referenced by this cursor.
 *)
function clang_getCursorSpelling(p1: TCXCursor): TCXString; cdecl external LIBCLANG;

(**
 * Retrieve a range for a piece that forms the cursors spelling name.
 * Most of the times there is only one range for the complete spelling but for
 * Objective-C methods and Objective-C message expressions, there are multiple
 * pieces for each selector identifier.
 *
 * \param pieceIndex the index of the spelling name piece. If this is greater
 * than the actual number of pieces, it will return a NULL (invalid) range.
 *
 * \param options Reserved.
 *)
function clang_Cursor_getSpellingNameRange(p1: TCXCursor; pieceIndex: Cardinal; options: Cardinal): TCXSourceRange; cdecl external LIBCLANG;

type
  (**
   * Opaque pointer representing a policy that controls pretty printing
   * for \c clang_getCursorPrettyPrinted.
   *)
  TCXPrintingPolicy = Pointer;

type
  (**
   * Properties for the printing policy.
   *
   * See \c clang::PrintingPolicy for more information.
   *)
  TCXPrintingPolicyProperty = Integer;

const
  CXPrintingPolicy_Indentation = 0;
  CXPrintingPolicy_SuppressSpecifiers = 1;
  CXPrintingPolicy_SuppressTagKeyword = 2;
  CXPrintingPolicy_IncludeTagDefinition = 3;
  CXPrintingPolicy_SuppressScope = 4;
  CXPrintingPolicy_SuppressUnwrittenScope = 5;
  CXPrintingPolicy_SuppressInitializers = 6;
  CXPrintingPolicy_ConstantArraySizeAsWritten = 7;
  CXPrintingPolicy_AnonymousTagLocations = 8;
  CXPrintingPolicy_SuppressStrongLifetime = 9;
  CXPrintingPolicy_SuppressLifetimeQualifiers = 10;
  CXPrintingPolicy_SuppressTemplateArgsInCXXConstructors = 11;
  CXPrintingPolicy_Bool = 12;
  CXPrintingPolicy_Restrict = 13;
  CXPrintingPolicy_Alignof = 14;
  CXPrintingPolicy_UnderscoreAlignof = 15;
  CXPrintingPolicy_UseVoidForZeroParams = 16;
  CXPrintingPolicy_TerseOutput = 17;
  CXPrintingPolicy_PolishForDeclaration = 18;
  CXPrintingPolicy_Half = 19;
  CXPrintingPolicy_MSWChar = 20;
  CXPrintingPolicy_IncludeNewlines = 21;
  CXPrintingPolicy_MSVCFormatting = 22;
  CXPrintingPolicy_ConstantsAsWritten = 23;
  CXPrintingPolicy_SuppressImplicitBase = 24;
  CXPrintingPolicy_FullyQualifiedName = 25;
  CXPrintingPolicy_LastProperty = CXPrintingPolicy_FullyQualifiedName;

(**
 * Get a property value for the given printing policy.
 *)
function clang_PrintingPolicy_getProperty(Policy: TCXPrintingPolicy; _Property: TCXPrintingPolicyProperty): Cardinal; cdecl external LIBCLANG;

(**
 * Set a property value for the given printing policy.
 *)
procedure clang_PrintingPolicy_setProperty(Policy: TCXPrintingPolicy; _Property: TCXPrintingPolicyProperty; Value: Cardinal); cdecl external LIBCLANG;

(**
 * Retrieve the default policy for the cursor.
 *
 * The policy should be released after use with \c
 * clang_PrintingPolicy_dispose.
 *)
function clang_getCursorPrintingPolicy(p1: TCXCursor): TCXPrintingPolicy; cdecl external LIBCLANG;

(**
 * Release a printing policy.
 *)
procedure clang_PrintingPolicy_dispose(Policy: TCXPrintingPolicy); cdecl external LIBCLANG;

(**
 * Pretty print declarations.
 *
 * \param Cursor The cursor representing a declaration.
 *
 * \param Policy The policy to control the entities being printed. If
 * NULL, a default policy is used.
 *
 * \returns The pretty printed declaration or the empty string for
 * other cursors.
 *)
function clang_getCursorPrettyPrinted(Cursor: TCXCursor; Policy: TCXPrintingPolicy): TCXString; cdecl external LIBCLANG;

(**
 * Retrieve the display name for the entity referenced by this cursor.
 *
 * The display name contains extra information that helps identify the cursor,
 * such as the parameters of a function or template or the arguments of a
 * class template specialization.
 *)
function clang_getCursorDisplayName(p1: TCXCursor): TCXString; cdecl external LIBCLANG;

(** For a cursor that is a reference, retrieve a cursor representing the
 * entity that it references.
 *
 * Reference cursors refer to other entities in the AST. For example, an
 * Objective-C superclass reference cursor refers to an Objective-C class.
 * This function produces the cursor for the Objective-C class from the
 * cursor for the superclass reference. If the input cursor is a declaration or
 * definition, it returns that declaration or definition unchanged.
 * Otherwise, returns the NULL cursor.
 *)
function clang_getCursorReferenced(p1: TCXCursor): TCXCursor; cdecl external LIBCLANG;

(**
 *  For a cursor that is either a reference to or a declaration
 *  of some entity, retrieve a cursor that describes the definition of
 *  that entity.
 *
 *  Some entities can be declared multiple times within a translation
 *  unit, but only one of those declarations can also be a
 *  definition. For example, given:
 *
 *  \code
 *  int f(int, int);
 *  int g(int x, int y) { return f(x, y); }
 *  int f(int a, int b) { return a + b; }
 *  int f(int, int);
 *  \endcode
 *
 *  there are three declarations of the function "f", but only the
 *  second one is a definition. The clang_getCursorDefinition()
 *  function will take any cursor pointing to a declaration of "f"
 *  (the first or fourth lines of the example) or a cursor referenced
 *  that uses "f" (the call to "f' inside "g") and will return a
 *  declaration cursor pointing to the definition (the second "f"
 *  declaration).
 *
 *  If given a cursor for which there is no corresponding definition,
 *  e.g., because there is no definition of that entity within this
 *  translation unit, returns a NULL cursor.
 *)
function clang_getCursorDefinition(p1: TCXCursor): TCXCursor; cdecl external LIBCLANG;

(**
 * Determine whether the declaration pointed to by this cursor
 * is also a definition of that entity.
 *)
function clang_isCursorDefinition(p1: TCXCursor): Cardinal; cdecl external LIBCLANG;

(**
 * Retrieve the canonical cursor corresponding to the given cursor.
 *
 * In the C family of languages, many kinds of entities can be declared several
 * times within a single translation unit. For example, a structure type can
 * be forward-declared (possibly multiple times) and later defined:
 *
 * \code
 * struct X;
 * struct X;
 * struct X {
 *   int member;
 * };
 * \endcode
 *
 * The declarations and the definition of \c X are represented by three
 * different cursors, all of which are declarations of the same underlying
 * entity. One of these cursor is considered the "canonical" cursor, which
 * is effectively the representative for the underlying entity. One can
 * determine if two cursors are declarations of the same underlying entity by
 * comparing their canonical cursors.
 *
 * \returns The canonical cursor for the entity referred to by the given cursor.
 *)
function clang_getCanonicalCursor(p1: TCXCursor): TCXCursor; cdecl external LIBCLANG;

(**
 * If the cursor points to a selector identifier in an Objective-C
 * method or message expression, this returns the selector index.
 *
 * After getting a cursor with #clang_getCursor, this can be called to
 * determine if the location points to a selector identifier.
 *
 * \returns The selector index if the cursor is an Objective-C method or message
 * expression and the cursor is pointing to a selector identifier, or -1
 * otherwise.
 *)
function clang_Cursor_getObjCSelectorIndex(p1: TCXCursor): Integer; cdecl external LIBCLANG;

(**
 * Given a cursor pointing to a C++ method call or an Objective-C
 * message, returns non-zero if the method/message is "dynamic", meaning:
 *
 * For a C++ method: the call is virtual.
 * For an Objective-C message: the receiver is an object instance, not 'super'
 * or a specific class.
 *
 * If the method/message is "static" or the cursor does not point to a
 * method/message, it will return zero.
 *)
function clang_Cursor_isDynamicCall(C: TCXCursor): Integer; cdecl external LIBCLANG;

(**
 * Given a cursor pointing to an Objective-C message or property
 * reference, or C++ method call, returns the CXType of the receiver.
 *)
function clang_Cursor_getReceiverType(C: TCXCursor): TCXType; cdecl external LIBCLANG;

(**
 * Property attributes for a \c CXCursor_ObjCPropertyDecl.
 *)
type
  TCXObjCPropertyAttrKind = Integer;

const
  CXObjCPropertyAttr_noattr = $00;
  CXObjCPropertyAttr_readonly = $01;
  CXObjCPropertyAttr_getter = $02;
  CXObjCPropertyAttr_assign = $04;
  CXObjCPropertyAttr_readwrite = $08;
  CXObjCPropertyAttr_retain = $10;
  CXObjCPropertyAttr_copy = $20;
  CXObjCPropertyAttr_nonatomic = $40;
  CXObjCPropertyAttr_setter = $80;
  CXObjCPropertyAttr_atomic = $100;
  CXObjCPropertyAttr_weak = $200;
  CXObjCPropertyAttr_strong = $400;
  CXObjCPropertyAttr_unsafe_unretained = $800;
  CXObjCPropertyAttr_class = $1000;

(**
 * Given a cursor that represents a property declaration, return the
 * associated property attributes. The bits are formed from
 * \c CXObjCPropertyAttrKind.
 *
 * \param reserved Reserved for future use, pass 0.
 *)
function clang_Cursor_getObjCPropertyAttributes(C: TCXCursor; reserved: Cardinal): Cardinal; cdecl external LIBCLANG;

(**
 * Given a cursor that represents a property declaration, return the
 * name of the method that implements the getter.
 *)
function clang_Cursor_getObjCPropertyGetterName(C: TCXCursor): TCXString; cdecl external LIBCLANG;

(**
 * Given a cursor that represents a property declaration, return the
 * name of the method that implements the setter, if any.
 *)
function clang_Cursor_getObjCPropertySetterName(C: TCXCursor): TCXString; cdecl external LIBCLANG;

(**
 * 'Qualifiers' written next to the return and parameter types in
 * Objective-C method declarations.
 *)
type
  TCXObjCDeclQualifierKind = Integer;

const
  CXObjCDeclQualifier_None = $0;
  CXObjCDeclQualifier_In = $1;
  CXObjCDeclQualifier_Inout = $2;
  CXObjCDeclQualifier_Out = $4;
  CXObjCDeclQualifier_Bycopy = $8;
  CXObjCDeclQualifier_Byref = $10;
  CXObjCDeclQualifier_Oneway = $20;

(**
 * Given a cursor that represents an Objective-C method or parameter
 * declaration, return the associated Objective-C qualifiers for the return
 * type or the parameter respectively. The bits are formed from
 * CXObjCDeclQualifierKind.
 *)
function clang_Cursor_getObjCDeclQualifiers(C: TCXCursor): Cardinal; cdecl external LIBCLANG;

(**
 * Given a cursor that represents an Objective-C method or property
 * declaration, return non-zero if the declaration was affected by "\@optional".
 * Returns zero if the cursor is not such a declaration or it is "\@required".
 *)
function clang_Cursor_isObjCOptional(C: TCXCursor): Cardinal; cdecl external LIBCLANG;

(**
 * Returns non-zero if the given cursor is a variadic function or method.
 *)
function clang_Cursor_isVariadic(C: TCXCursor): Cardinal; cdecl external LIBCLANG;

(**
 * Returns non-zero if the given cursor points to a symbol marked with
 * external_source_symbol attribute.
 *
 * \param language If non-NULL, and the attribute is present, will be set to
 * the 'language' string from the attribute.
 *
 * \param definedIn If non-NULL, and the attribute is present, will be set to
 * the 'definedIn' string from the attribute.
 *
 * \param isGenerated If non-NULL, and the attribute is present, will be set to
 * non-zero if the 'generated_declaration' is set in the attribute.
 *)
function clang_Cursor_isExternalSymbol(C: TCXCursor; language: PCXString; definedIn: PCXString; isGenerated: PCardinal): Cardinal; cdecl external LIBCLANG;

(**
 * Given a cursor that represents a declaration, return the associated
 * comment's source range.  The range may include multiple consecutive comments
 * with whitespace in between.
 *)
function clang_Cursor_getCommentRange(C: TCXCursor): TCXSourceRange; cdecl external LIBCLANG;

(**
 * Given a cursor that represents a declaration, return the associated
 * comment text, including comment markers.
 *)
function clang_Cursor_getRawCommentText(C: TCXCursor): TCXString; cdecl external LIBCLANG;

(**
 * Given a cursor that represents a documentable entity (e.g.,
 * declaration), return the associated \paragraph; otherwise return the
 * first paragraph.
 *)
function clang_Cursor_getBriefCommentText(C: TCXCursor): TCXString; cdecl external LIBCLANG;

(**
 * @}
 *)

(** \defgroup CINDEX_MANGLE Name Mangling API Functions
 *
 * @{
 *)

(**
 * Retrieve the CXString representing the mangled name of the cursor.
 *)
function clang_Cursor_getMangling(p1: TCXCursor): TCXString; cdecl external LIBCLANG;

(**
 * Retrieve the CXStrings representing the mangled symbols of the C++
 * constructor or destructor at the cursor.
 *)
function clang_Cursor_getCXXManglings(p1: TCXCursor): PCXStringSet; cdecl external LIBCLANG;

(**
 * Retrieve the CXStrings representing the mangled symbols of the ObjC
 * class interface or implementation at the cursor.
 *)
function clang_Cursor_getObjCManglings(p1: TCXCursor): PCXStringSet; cdecl external LIBCLANG;

(**
 * @}
 *)

(**
 * \defgroup CINDEX_MODULE Module introspection
 *
 * The functions in this group provide access to information about modules.
 *
 * @{
 *)

type TCXModule = Pointer;

(**
 * Given a CXCursor_ModuleImportDecl cursor, return the associated module.
 *)
function clang_Cursor_getModule(C: TCXCursor): TCXModule; cdecl external LIBCLANG;

(**
 * Given a CXFile header file, return the module that contains it, if one
 * exists.
 *)
function clang_getModuleForFile(p1: TCXTranslationUnit; p2: TCXFile): TCXModule; cdecl external LIBCLANG;

(**
 * \param Module a module object.
 *
 * \returns the module file where the provided module object came from.
 *)
function clang_Module_getASTFile(Module: TCXModule): TCXFile; cdecl external LIBCLANG;

(**
 * \param Module a module object.
 *
 * \returns the parent of a sub-module or NULL if the given module is top-level,
 * e.g. for 'std.vector' it will return the 'std' module.
 *)
function clang_Module_getParent(Module: TCXModule): TCXModule; cdecl external LIBCLANG;

(**
 * \param Module a module object.
 *
 * \returns the name of the module, e.g. for the 'std.vector' sub-module it
 * will return "vector".
 *)
function clang_Module_getName(Module: TCXModule): TCXString; cdecl external LIBCLANG;

(**
 * \param Module a module object.
 *
 * \returns the full name of the module, e.g. "std.vector".
 *)
function clang_Module_getFullName(Module: TCXModule): TCXString; cdecl external LIBCLANG;

(**
 * \param Module a module object.
 *
 * \returns non-zero if the module is a system one.
 *)
function clang_Module_isSystem(Module: TCXModule): Integer; cdecl external LIBCLANG;

(**
 * \param Module a module object.
 *
 * \returns the number of top level headers associated with this module.
 *)
function clang_Module_getNumTopLevelHeaders(p1: TCXTranslationUnit; Module: TCXModule): Cardinal; cdecl external LIBCLANG;

(**
 * \param Module a module object.
 *
 * \param Index top level header index (zero-based).
 *
 * \returns the specified top level header associated with the module.
 *)
function clang_Module_getTopLevelHeader(p1: TCXTranslationUnit; Module: TCXModule; Index: Cardinal): TCXFile; cdecl external LIBCLANG;

(**
 * @}
 *)

(**
 * \defgroup CINDEX_CPP C++ AST introspection
 *
 * The routines in this group provide access information in the ASTs specific
 * to C++ language features.
 *
 * @{
 *)

(**
 * Determine if a C++ constructor is a converting constructor.
 *)
function clang_CXXConstructor_isConvertingConstructor(C: TCXCursor): Cardinal; cdecl external LIBCLANG;

(**
 * Determine if a C++ constructor is a copy constructor.
 *)
function clang_CXXConstructor_isCopyConstructor(C: TCXCursor): Cardinal; cdecl external LIBCLANG;

(**
 * Determine if a C++ constructor is the default constructor.
 *)
function clang_CXXConstructor_isDefaultConstructor(C: TCXCursor): Cardinal; cdecl external LIBCLANG;

(**
 * Determine if a C++ constructor is a move constructor.
 *)
function clang_CXXConstructor_isMoveConstructor(C: TCXCursor): Cardinal; cdecl external LIBCLANG;

(**
 * Determine if a C++ field is declared 'mutable'.
 *)
function clang_CXXField_isMutable(C: TCXCursor): Cardinal; cdecl external LIBCLANG;

(**
 * Determine if a C++ method is declared '= default'.
 *)
function clang_CXXMethod_isDefaulted(C: TCXCursor): Cardinal; cdecl external LIBCLANG;

(**
 * Determine if a C++ member function or member function template is
 * pure virtual.
 *)
function clang_CXXMethod_isPureVirtual(C: TCXCursor): Cardinal; cdecl external LIBCLANG;

(**
 * Determine if a C++ member function or member function template is
 * declared 'static'.
 *)
function clang_CXXMethod_isStatic(C: TCXCursor): Cardinal; cdecl external LIBCLANG;

(**
 * Determine if a C++ member function or member function template is
 * explicitly declared 'virtual' or if it overrides a virtual method from
 * one of the base classes.
 *)
function clang_CXXMethod_isVirtual(C: TCXCursor): Cardinal; cdecl external LIBCLANG;

(**
 * Determine if a C++ record is abstract, i.e. whether a class or struct
 * has a pure virtual member function.
 *)
function clang_CXXRecord_isAbstract(C: TCXCursor): Cardinal; cdecl external LIBCLANG;

(**
 * Determine if an enum declaration refers to a scoped enum.
 *)
function clang_EnumDecl_isScoped(C: TCXCursor): Cardinal; cdecl external LIBCLANG;

(**
 * Determine if a C++ member function or member function template is
 * declared 'const'.
 *)
function clang_CXXMethod_isConst(C: TCXCursor): Cardinal; cdecl external LIBCLANG;

(**
 * Given a cursor that represents a template, determine
 * the cursor kind of the specializations would be generated by instantiating
 * the template.
 *
 * This routine can be used to determine what flavor of function template,
 * class template, or class template partial specialization is stored in the
 * cursor. For example, it can describe whether a class template cursor is
 * declared with "struct", "class" or "union".
 *
 * \param C The cursor to query. This cursor should represent a template
 * declaration.
 *
 * \returns The cursor kind of the specializations that would be generated
 * by instantiating the template \p C. If \p C is not a template, returns
 * \c CXCursor_NoDeclFound.
 *)
function clang_getTemplateCursorKind(C: TCXCursor): TCXCursorKind; cdecl external LIBCLANG;

(**
 * Given a cursor that may represent a specialization or instantiation
 * of a template, retrieve the cursor that represents the template that it
 * specializes or from which it was instantiated.
 *
 * This routine determines the template involved both for explicit
 * specializations of templates and for implicit instantiations of the template,
 * both of which are referred to as "specializations". For a class template
 * specialization (e.g., \c std::vector<bool>), this routine will return
 * either the primary template (\c std::vector) or, if the specialization was
 * instantiated from a class template partial specialization, the class template
 * partial specialization. For a class template partial specialization and a
 * function template specialization (including instantiations), this
 * this routine will return the specialized template.
 *
 * For members of a class template (e.g., member functions, member classes, or
 * static data members), returns the specialized or instantiated member.
 * Although not strictly "templates" in the C++ language, members of class
 * templates have the same notions of specializations and instantiations that
 * templates do, so this routine treats them similarly.
 *
 * \param C A cursor that may be a specialization of a template or a member
 * of a template.
 *
 * \returns If the given cursor is a specialization or instantiation of a
 * template or a member thereof, the template or member that it specializes or
 * from which it was instantiated. Otherwise, returns a NULL cursor.
 *)
function clang_getSpecializedCursorTemplate(C: TCXCursor): TCXCursor; cdecl external LIBCLANG;

(**
 * Given a cursor that references something else, return the source range
 * covering that reference.
 *
 * \param C A cursor pointing to a member reference, a declaration reference, or
 * an operator call.
 * \param NameFlags A bitset with three independent flags:
 * CXNameRange_WantQualifier, CXNameRange_WantTemplateArgs, and
 * CXNameRange_WantSinglePiece.
 * \param PieceIndex For contiguous names or when passing the flag
 * CXNameRange_WantSinglePiece, only one piece with index 0 is
 * available. When the CXNameRange_WantSinglePiece flag is not passed for a
 * non-contiguous names, this index can be used to retrieve the individual
 * pieces of the name. See also CXNameRange_WantSinglePiece.
 *
 * \returns The piece of the name pointed to by the given cursor. If there is no
 * name, or if the PieceIndex is out-of-range, a null-cursor will be returned.
 *)
function clang_getCursorReferenceNameRange(C: TCXCursor; NameFlags: Cardinal; PieceIndex: Cardinal): TCXSourceRange; cdecl external LIBCLANG;

type
  TCXNameRefFlags = Integer;
  PCXNameRefFlags = ^TCXNameRefFlags;

const
  CXNameRange_WantQualifier = $1;
  CXNameRange_WantTemplateArgs = $2;
  CXNameRange_WantSinglePiece = $4;

(**
 * @}
 *)

(**
 * \defgroup CINDEX_LEX Token extraction and manipulation
 *
 * The routines in this group provide access to the tokens within a
 * translation unit, along with a semantic mapping of those tokens to
 * their corresponding cursors.
 *
 * @{
 *)

(**
 * Describes a kind of token.
 *)
type
  TCXTokenKind = Integer;

const
  CXToken_Punctuation = 0;
  CXToken_Keyword = CXToken_Punctuation + 1;
  CXToken_Identifier = CXToken_Keyword + 1;
  CXToken_Literal = CXToken_Identifier + 1;
  CXToken_Comment = CXToken_Literal + 1;

(**
 * Describes a single preprocessing token.
 *)
type
  TCXToken = record
    int_data: array [0..4-1] of Cardinal;
    ptr_data: Pointer;
  end;
  PCXToken = ^TCXToken;
  PPCXToken = ^PCXToken;

(**
 * Get the raw lexical token starting with the given location.
 *
 * \param TU the translation unit whose text is being tokenized.
 *
 * \param Location the source location with which the token starts.
 *
 * \returns The token starting with the given location or NULL if no such token
 * exist. The returned pointer must be freed with clang_disposeTokens before the
 * translation unit is destroyed.
 *)
function clang_getToken(TU: TCXTranslationUnit; Location: TCXSourceLocation): PCXToken; cdecl external LIBCLANG;

(**
 * Determine the kind of the given token.
 *)
function clang_getTokenKind(p1: TCXToken): TCXTokenKind; cdecl external LIBCLANG;

(**
 * Determine the spelling of the given token.
 *
 * The spelling of a token is the textual representation of that token, e.g.,
 * the text of an identifier or keyword.
 *)
function clang_getTokenSpelling(p1: TCXTranslationUnit; p2: TCXToken): TCXString; cdecl external LIBCLANG;

(**
 * Retrieve the source location of the given token.
 *)
function clang_getTokenLocation(p1: TCXTranslationUnit; p2: TCXToken): TCXSourceLocation; cdecl external LIBCLANG;

(**
 * Retrieve a source range that covers the given token.
 *)
function clang_getTokenExtent(p1: TCXTranslationUnit; p2: TCXToken): TCXSourceRange; cdecl external LIBCLANG;

(**
 * Tokenize the source code described by the given range into raw
 * lexical tokens.
 *
 * \param TU the translation unit whose text is being tokenized.
 *
 * \param Range the source range in which text should be tokenized. All of the
 * tokens produced by tokenization will fall within this source range,
 *
 * \param Tokens this pointer will be set to point to the array of tokens
 * that occur within the given source range. The returned pointer must be
 * freed with clang_disposeTokens() before the translation unit is destroyed.
 *
 * \param NumTokens will be set to the number of tokens in the \c *Tokens
 * array.
 *
 *)
procedure clang_tokenize(TU: TCXTranslationUnit; Range: TCXSourceRange; Tokens: PPCXToken; NumTokens: PCardinal); cdecl external LIBCLANG;

(**
 * Annotate the given set of tokens by providing cursors for each token
 * that can be mapped to a specific entity within the abstract syntax tree.
 *
 * This token-annotation routine is equivalent to invoking
 * clang_getCursor() for the source locations of each of the
 * tokens. The cursors provided are filtered, so that only those
 * cursors that have a direct correspondence to the token are
 * accepted. For example, given a function call \c f(x),
 * clang_getCursor() would provide the following cursors:
 *
 *   * when the cursor is over the 'f', a DeclRefExpr cursor referring to 'f'.
 *   * when the cursor is over the '(' or the ')', a CallExpr referring to 'f'.
 *   * when the cursor is over the 'x', a DeclRefExpr cursor referring to 'x'.
 *
 * Only the first and last of these cursors will occur within the
 * annotate, since the tokens "f" and "x' directly refer to a function
 * and a variable, respectively, but the parentheses are just a small
 * part of the full syntax of the function call expression, which is
 * not provided as an annotation.
 *
 * \param TU the translation unit that owns the given tokens.
 *
 * \param Tokens the set of tokens to annotate.
 *
 * \param NumTokens the number of tokens in \p Tokens.
 *
 * \param Cursors an array of \p NumTokens cursors, whose contents will be
 * replaced with the cursors corresponding to each token.
 *)
procedure clang_annotateTokens(TU: TCXTranslationUnit; Tokens: PCXToken; NumTokens: Cardinal; Cursors: PCXCursor); cdecl external LIBCLANG;

(**
 * Free the given set of tokens.
 *)
procedure clang_disposeTokens(TU: TCXTranslationUnit; Tokens: PCXToken; NumTokens: Cardinal); cdecl external LIBCLANG;

(**
 * @}
 *)

(**
 * \defgroup CINDEX_DEBUG Debugging facilities
 *
 * These routines are used for testing and debugging, only, and should not
 * be relied upon.
 *
 * @{
 *)

(* for debug/testing *)
type
  Tclang_threadFunc = procedure(user_data: Pointer); cdecl;

function clang_getCursorKindSpelling(Kind: TCXCursorKind): TCXString; cdecl external LIBCLANG;
procedure clang_getDefinitionSpellingAndExtent(p1: TCXCursor; const startBuf: PPAnsiChar; const endBuf: PPAnsiChar; startLine: PCardinal; startColumn: PCardinal; endLine: PCardinal; endColumn: PCardinal); cdecl external LIBCLANG;
procedure clang_enableStackTraces(); cdecl external LIBCLANG;
procedure clang_executeOnThread(func: Tclang_threadFunc; user_data: Pointer; stack_size: Cardinal); cdecl external LIBCLANG;

(**
 * @}
 *)

(**
 * \defgroup CINDEX_CODE_COMPLET Code completion
 *
 * Code completion involves taking an (incomplete) source file, along with
 * knowledge of where the user is actively editing that file, and suggesting
 * syntactically- and semantically-valid constructs that the user might want to
 * use at that particular point in the source code. These data structures and
 * routines provide support for code completion.
 *
 * @{
 *)

(**
 * A semantic string that describes a code-completion result.
 *
 * A semantic string that describes the formatting of a code-completion
 * result as a single "template" of text that should be inserted into the
 * source buffer when a particular code-completion result is selected.
 * Each semantic string is made up of some number of "chunks", each of which
 * contains some text along with a description of what that text means, e.g.,
 * the name of the entity being referenced, whether the text chunk is part of
 * the template, or whether it is a "placeholder" that the user should replace
 * with actual code,of a specific kind. See \c CXCompletionChunkKind for a
 * description of the different kinds of chunks.
 *)
type TCXCompletionString = Pointer;

(**
 * A single result of code completion.
 *)
type
  TCXCompletionResult = record
    CursorKind: TCXCursorKind;
    CompletionString: TCXCompletionString;
  end;
  PCXCompletionResult = ^TCXCompletionResult;

(**
 * Describes a single piece of text within a code-completion string.
 *
 * Each "chunk" within a code-completion string (\c CXCompletionString) is
 * either a piece of text with a specific "kind" that describes how that text
 * should be interpreted by the client or is another completion string.
 *)
type
  TCXCompletionChunkKind = Integer;

const
  CXCompletionChunk_Optional = 0;
  CXCompletionChunk_TypedText = CXCompletionChunk_Optional + 1;
  CXCompletionChunk_Text = CXCompletionChunk_TypedText + 1;
  CXCompletionChunk_Placeholder = CXCompletionChunk_Text + 1;
  CXCompletionChunk_Informative = CXCompletionChunk_Placeholder + 1;
  CXCompletionChunk_CurrentParameter = CXCompletionChunk_Informative + 1;
  CXCompletionChunk_LeftParen = CXCompletionChunk_CurrentParameter + 1;
  CXCompletionChunk_RightParen = CXCompletionChunk_LeftParen + 1;
  CXCompletionChunk_LeftBracket = CXCompletionChunk_RightParen + 1;
  CXCompletionChunk_RightBracket = CXCompletionChunk_LeftBracket + 1;
  CXCompletionChunk_LeftBrace = CXCompletionChunk_RightBracket + 1;
  CXCompletionChunk_RightBrace = CXCompletionChunk_LeftBrace + 1;
  CXCompletionChunk_LeftAngle = CXCompletionChunk_RightBrace + 1;
  CXCompletionChunk_RightAngle = CXCompletionChunk_LeftAngle + 1;
  CXCompletionChunk_Comma = CXCompletionChunk_RightAngle + 1;
  CXCompletionChunk_ResultType = CXCompletionChunk_Comma + 1;
  CXCompletionChunk_Colon = CXCompletionChunk_ResultType + 1;
  CXCompletionChunk_SemiColon = CXCompletionChunk_Colon + 1;
  CXCompletionChunk_Equal = CXCompletionChunk_SemiColon + 1;
  CXCompletionChunk_HorizontalSpace = CXCompletionChunk_Equal + 1;
  CXCompletionChunk_VerticalSpace = CXCompletionChunk_HorizontalSpace + 1;

(**
 * Determine the kind of a particular chunk within a completion string.
 *
 * \param completion_string the completion string to query.
 *
 * \param chunk_number the 0-based index of the chunk in the completion string.
 *
 * \returns the kind of the chunk at the index \c chunk_number.
 *)
function clang_getCompletionChunkKind(completion_string: TCXCompletionString; chunk_number: Cardinal): TCXCompletionChunkKind; cdecl external LIBCLANG;

(**
 * Retrieve the text associated with a particular chunk within a
 * completion string.
 *
 * \param completion_string the completion string to query.
 *
 * \param chunk_number the 0-based index of the chunk in the completion string.
 *
 * \returns the text associated with the chunk at index \c chunk_number.
 *)
function clang_getCompletionChunkText(completion_string: TCXCompletionString; chunk_number: Cardinal): TCXString; cdecl external LIBCLANG;

(**
 * Retrieve the completion string associated with a particular chunk
 * within a completion string.
 *
 * \param completion_string the completion string to query.
 *
 * \param chunk_number the 0-based index of the chunk in the completion string.
 *
 * \returns the completion string associated with the chunk at index
 * \c chunk_number.
 *)
function clang_getCompletionChunkCompletionString(completion_string: TCXCompletionString; chunk_number: Cardinal): TCXCompletionString; cdecl external LIBCLANG;

(**
 * Retrieve the number of chunks in the given code-completion string.
 *)
function clang_getNumCompletionChunks(completion_string: TCXCompletionString): Cardinal; cdecl external LIBCLANG;

(**
 * Determine the priority of this code completion.
 *
 * The priority of a code completion indicates how likely it is that this
 * particular completion is the completion that the user will select. The
 * priority is selected by various internal heuristics.
 *
 * \param completion_string The completion string to query.
 *
 * \returns The priority of this completion string. Smaller values indicate
 * higher-priority (more likely) completions.
 *)
function clang_getCompletionPriority(completion_string: TCXCompletionString): Cardinal; cdecl external LIBCLANG;

(**
 * Determine the availability of the entity that this code-completion
 * string refers to.
 *
 * \param completion_string The completion string to query.
 *
 * \returns The availability of the completion string.
 *)
function clang_getCompletionAvailability(completion_string: TCXCompletionString): TCXAvailabilityKind; cdecl external LIBCLANG;

(**
 * Retrieve the number of annotations associated with the given
 * completion string.
 *
 * \param completion_string the completion string to query.
 *
 * \returns the number of annotations associated with the given completion
 * string.
 *)
function clang_getCompletionNumAnnotations(completion_string: TCXCompletionString): Cardinal; cdecl external LIBCLANG;

(**
 * Retrieve the annotation associated with the given completion string.
 *
 * \param completion_string the completion string to query.
 *
 * \param annotation_number the 0-based index of the annotation of the
 * completion string.
 *
 * \returns annotation string associated with the completion at index
 * \c annotation_number, or a NULL string if that annotation is not available.
 *)
function clang_getCompletionAnnotation(completion_string: TCXCompletionString; annotation_number: Cardinal): TCXString; cdecl external LIBCLANG;

(**
 * Retrieve the parent context of the given completion string.
 *
 * The parent context of a completion string is the semantic parent of
 * the declaration (if any) that the code completion represents. For example,
 * a code completion for an Objective-C method would have the method's class
 * or protocol as its context.
 *
 * \param completion_string The code completion string whose parent is
 * being queried.
 *
 * \param kind DEPRECATED: always set to CXCursor_NotImplemented if non-NULL.
 *
 * \returns The name of the completion parent, e.g., "NSObject" if
 * the completion string represents a method in the NSObject class.
 *)
function clang_getCompletionParent(completion_string: TCXCompletionString; kind: PCXCursorKind): TCXString; cdecl external LIBCLANG;

(**
 * Retrieve the brief documentation comment attached to the declaration
 * that corresponds to the given completion string.
 *)
function clang_getCompletionBriefComment(completion_string: TCXCompletionString): TCXString; cdecl external LIBCLANG;

(**
 * Retrieve a completion string for an arbitrary declaration or macro
 * definition cursor.
 *
 * \param cursor The cursor to query.
 *
 * \returns A non-context-sensitive completion string for declaration and macro
 * definition cursors, or NULL for other kinds of cursors.
 *)
function clang_getCursorCompletionString(cursor: TCXCursor): TCXCompletionString; cdecl external LIBCLANG;

(**
 * Contains the results of code-completion.
 *
 * This data structure contains the results of code completion, as
 * produced by \c clang_codeCompleteAt(). Its contents must be freed by
 * \c clang_disposeCodeCompleteResults.
 *)
type
  TCXCodeCompleteResults = record
    Results: PCXCompletionResult;
    NumResults: Cardinal;
  end;
  PCXCodeCompleteResults = ^TCXCodeCompleteResults;

(**
 * Retrieve the number of fix-its for the given completion index.
 *
 * Calling this makes sense only if CXCodeComplete_IncludeCompletionsWithFixIts
 * option was set.
 *
 * \param results The structure keeping all completion results
 *
 * \param completion_index The index of the completion
 *
 * \return The number of fix-its which must be applied before the completion at
 * completion_index can be applied
 *)
function clang_getCompletionNumFixIts(results: PCXCodeCompleteResults; completion_index: Cardinal): Cardinal; cdecl external LIBCLANG;

(**
 * Fix-its that *must* be applied before inserting the text for the
 * corresponding completion.
 *
 * By default, clang_codeCompleteAt() only returns completions with empty
 * fix-its. Extra completions with non-empty fix-its should be explicitly
 * requested by setting CXCodeComplete_IncludeCompletionsWithFixIts.
 *
 * For the clients to be able to compute position of the cursor after applying
 * fix-its, the following conditions are guaranteed to hold for
 * replacement_range of the stored fix-its:
 *  - Ranges in the fix-its are guaranteed to never contain the completion
 *  point (or identifier under completion point, if any) inside them, except
 *  at the start or at the end of the range.
 *  - If a fix-it range starts or ends with completion point (or starts or
 *  ends after the identifier under completion point), it will contain at
 *  least one character. It allows to unambiguously recompute completion
 *  point after applying the fix-it.
 *
 * The intuition is that provided fix-its change code around the identifier we
 * complete, but are not allowed to touch the identifier itself or the
 * completion point. One example of completions with corrections are the ones
 * replacing '.' with '->' and vice versa:
 *
 * std::unique_ptr<std::vector<int>> vec_ptr;
 * In 'vec_ptr.^', one of the completions is 'push_back', it requires
 * replacing '.' with '->'.
 * In 'vec_ptr->^', one of the completions is 'release', it requires
 * replacing '->' with '.'.
 *
 * \param results The structure keeping all completion results
 *
 * \param completion_index The index of the completion
 *
 * \param fixit_index The index of the fix-it for the completion at
 * completion_index
 *
 * \param replacement_range The fix-it range that must be replaced before the
 * completion at completion_index can be applied
 *
 * \returns The fix-it string that must replace the code at replacement_range
 * before the completion at completion_index can be applied
 *)
function clang_getCompletionFixIt(results: PCXCodeCompleteResults; completion_index, fixit_index: Cardinal; replacement_range: PCXSourceRange): TCXString; cdecl external LIBCLANG;

(**
 * Flags that can be passed to \c clang_codeCompleteAt() to
 * modify its behavior.
 *
 * The enumerators in this enumeration can be bitwise-OR'd together to
 * provide multiple options to \c clang_codeCompleteAt().
 *)
type
  TCXCodeComplete_Flags = Integer;

const
  CXCodeComplete_IncludeMacros = $01;
  CXCodeComplete_IncludeCodePatterns = $02;
  CXCodeComplete_IncludeBriefComments = $04;
  CXCodeComplete_SkipPreamble = $08;
  CXCodeComplete_IncludeCompletionsWithFixIts = $10;

(**
 * Bits that represent the context under which completion is occurring.
 *
 * The enumerators in this enumeration may be bitwise-OR'd together if multiple
 * contexts are occurring simultaneously.
 *)
type
  TCXCompletionContext = Integer;

const
  CXCompletionContext_Unexposed = 0;
  CXCompletionContext_AnyType = 1  shl  0;
  CXCompletionContext_AnyValue = 1  shl  1;
  CXCompletionContext_ObjCObjectValue = 1  shl  2;
  CXCompletionContext_ObjCSelectorValue = 1  shl  3;
  CXCompletionContext_CXXClassTypeValue = 1  shl  4;
  CXCompletionContext_DotMemberAccess = 1  shl  5;
  CXCompletionContext_ArrowMemberAccess = 1  shl  6;
  CXCompletionContext_ObjCPropertyAccess = 1  shl  7;
  CXCompletionContext_EnumTag = 1  shl  8;
  CXCompletionContext_UnionTag = 1  shl  9;
  CXCompletionContext_StructTag = 1  shl  10;
  CXCompletionContext_ClassTag = 1  shl  11;
  CXCompletionContext_Namespace = 1  shl  12;
  CXCompletionContext_NestedNameSpecifier = 1  shl  13;
  CXCompletionContext_ObjCInterface = 1  shl  14;
  CXCompletionContext_ObjCProtocol = 1  shl  15;
  CXCompletionContext_ObjCCategory = 1  shl  16;
  CXCompletionContext_ObjCInstanceMessage = 1  shl  17;
  CXCompletionContext_ObjCClassMessage = 1  shl  18;
  CXCompletionContext_ObjCSelectorName = 1  shl  19;
  CXCompletionContext_MacroName = 1  shl  20;
  CXCompletionContext_NaturalLanguage = 1  shl  21;
  CXCompletionContext_IncludedFile = 1 shl 22;
  CXCompletionContext_Unknown = ((1 shl 23) - 1);

(**
 * Returns a default set of code-completion options that can be
 * passed to\c clang_codeCompleteAt().
 *)
function clang_defaultCodeCompleteOptions(): Cardinal; cdecl external LIBCLANG;

(**
 * Perform code completion at a given location in a translation unit.
 *
 * This function performs code completion at a particular file, line, and
 * column within source code, providing results that suggest potential
 * code snippets based on the context of the completion. The basic model
 * for code completion is that Clang will parse a complete source file,
 * performing syntax checking up to the location where code-completion has
 * been requested. At that point, a special code-completion token is passed
 * to the parser, which recognizes this token and determines, based on the
 * current location in the C/Objective-C/C++ grammar and the state of
 * semantic analysis, what completions to provide. These completions are
 * returned via a new \c CXCodeCompleteResults structure.
 *
 * Code completion itself is meant to be triggered by the client when the
 * user types punctuation characters or whitespace, at which point the
 * code-completion location will coincide with the cursor. For example, if \c p
 * is a pointer, code-completion might be triggered after the "-" and then
 * after the ">" in \c p->. When the code-completion location is afer the ">",
 * the completion results will provide, e.g., the members of the struct that
 * "p" points to. The client is responsible for placing the cursor at the
 * beginning of the token currently being typed, then filtering the results
 * based on the contents of the token. For example, when code-completing for
 * the expression \c p->get, the client should provide the location just after
 * the ">" (e.g., pointing at the "g") to this code-completion hook. Then, the
 * client can filter the results based on the current token text ("get"), only
 * showing those results that start with "get". The intent of this interface
 * is to separate the relatively high-latency acquisition of code-completion
 * results from the filtering of results on a per-character basis, which must
 * have a lower latency.
 *
 * \param TU The translation unit in which code-completion should
 * occur. The source files for this translation unit need not be
 * completely up-to-date (and the contents of those source files may
 * be overridden via \p unsaved_files). Cursors referring into the
 * translation unit may be invalidated by this invocation.
 *
 * \param complete_filename The name of the source file where code
 * completion should be performed. This filename may be any file
 * included in the translation unit.
 *
 * \param complete_line The line at which code-completion should occur.
 *
 * \param complete_column The column at which code-completion should occur.
 * Note that the column should point just after the syntactic construct that
 * initiated code completion, and not in the middle of a lexical token.
 *
 * \param unsaved_files the Files that have not yet been saved to disk
 * but may be required for parsing or code completion, including the
 * contents of those files.  The contents and name of these files (as
 * specified by CXUnsavedFile) are copied when necessary, so the
 * client only needs to guarantee their validity until the call to
 * this function returns.
 *
 * \param num_unsaved_files The number of unsaved file entries in \p
 * unsaved_files.
 *
 * \param options Extra options that control the behavior of code
 * completion, expressed as a bitwise OR of the enumerators of the
 * CXCodeComplete_Flags enumeration. The
 * \c clang_defaultCodeCompleteOptions() function returns a default set
 * of code-completion options.
 *
 * \returns If successful, a new \c CXCodeCompleteResults structure
 * containing code-completion results, which should eventually be
 * freed with \c clang_disposeCodeCompleteResults(). If code
 * completion fails, returns NULL.
 *)
function clang_codeCompleteAt(TU: TCXTranslationUnit; const complete_filename: PAnsiChar; complete_line: Cardinal; complete_column: Cardinal; unsaved_files: PCXUnsavedFile; num_unsaved_files: Cardinal; options: Cardinal): PCXCodeCompleteResults; cdecl external LIBCLANG;

(**
 * Sort the code-completion results in case-insensitive alphabetical
 * order.
 *
 * \param Results The set of results to sort.
 * \param NumResults The number of results in \p Results.
 *)
procedure clang_sortCodeCompletionResults(Results: PCXCompletionResult; NumResults: Cardinal); cdecl external LIBCLANG;

(**
 * Free the given set of code-completion results.
 *)
procedure clang_disposeCodeCompleteResults(Results: PCXCodeCompleteResults); cdecl external LIBCLANG;

(**
 * Determine the number of diagnostics produced prior to the
 * location where code completion was performed.
 *)
function clang_codeCompleteGetNumDiagnostics(Results: PCXCodeCompleteResults): Cardinal; cdecl external LIBCLANG;

(**
 * Retrieve a diagnostic associated with the given code completion.
 *
 * \param Results the code completion results to query.
 * \param Index the zero-based diagnostic number to retrieve.
 *
 * \returns the requested diagnostic. This diagnostic must be freed
 * via a call to \c clang_disposeDiagnostic().
 *)
function clang_codeCompleteGetDiagnostic(Results: PCXCodeCompleteResults; Index: Cardinal): TCXDiagnostic; cdecl external LIBCLANG;

(**
 * Determines what completions are appropriate for the context
 * the given code completion.
 *
 * \param Results the code completion results to query
 *
 * \returns the kinds of completions that are appropriate for use
 * along with the given code completion results.
 *)
function clang_codeCompleteGetContexts(Results: PCXCodeCompleteResults): UInt64; cdecl external LIBCLANG;

(**
 * Returns the cursor kind for the container for the current code
 * completion context. The container is only guaranteed to be set for
 * contexts where a container exists (i.e. member accesses or Objective-C
 * message sends); if there is not a container, this function will return
 * CXCursor_InvalidCode.
 *
 * \param Results the code completion results to query
 *
 * \param IsIncomplete on return, this value will be false if Clang has complete
 * information about the container. If Clang does not have complete
 * information, this value will be true.
 *
 * \returns the container kind, or CXCursor_InvalidCode if there is not a
 * container
 *)
function clang_codeCompleteGetContainerKind(Results: PCXCodeCompleteResults; IsIncomplete: PCardinal): TCXCursorKind; cdecl external LIBCLANG;

(**
 * Returns the USR for the container for the current code completion
 * context. If there is not a container for the current context, this
 * function will return the empty string.
 *
 * \param Results the code completion results to query
 *
 * \returns the USR for the container
 *)
function clang_codeCompleteGetContainerUSR(Results: PCXCodeCompleteResults): TCXString; cdecl external LIBCLANG;

(**
 * Returns the currently-entered selector for an Objective-C message
 * send, formatted like "initWithFoo:bar:". Only guaranteed to return a
 * non-empty string for CXCompletionContext_ObjCInstanceMessage and
 * CXCompletionContext_ObjCClassMessage.
 *
 * \param Results the code completion results to query
 *
 * \returns the selector (or partial selector) that has been entered thus far
 * for an Objective-C message send.
 *)
function clang_codeCompleteGetObjCSelector(Results: PCXCodeCompleteResults): TCXString; cdecl external LIBCLANG;

(**
 * @}
 *)

(**
 * \defgroup CINDEX_MISC Miscellaneous utility functions
 *
 * @{
 *)

(**
 * Return a version string, suitable for showing to a user, but not
 *        intended to be parsed (the format is not guaranteed to be stable).
 *)
function clang_getClangVersion(): TCXString; cdecl external LIBCLANG;

(**
 * Enable/disable crash recovery.
 *
 * \param isEnabled Flag to indicate if crash recovery is enabled.  A non-zero
 *        value enables crash recovery, while 0 disables it.
 *)
procedure clang_toggleCrashRecovery(isEnabled: Cardinal); cdecl external LIBCLANG;

 (**
  * Visitor invoked for each file in a translation unit
  *        (used with clang_getInclusions()).
  *
  * This visitor function will be invoked by clang_getInclusions() for each
  * file included (either at the top-level or by \#include directives) within
  * a translation unit.  The first argument is the file being included, and
  * the second and third arguments provide the inclusion stack.  The
  * array is sorted in order of immediate inclusion.  For example,
  * the first element refers to the location that included 'included_file'.
  *)
type TCXInclusionVisitor = procedure(included_file: TCXFile; inclusion_stack: PCXSourceLocation; include_len: Cardinal; client_data: TCXClientData); cdecl;

(**
 * Visit the set of preprocessor inclusions in a translation unit.
 *   The visitor function is called with the provided data for every included
 *   file.  This does not include headers included by the PCH file (unless one
 *   is inspecting the inclusions in the PCH file itself).
 *)
procedure clang_getInclusions(tu: TCXTranslationUnit; visitor: TCXInclusionVisitor; client_data: TCXClientData); cdecl external LIBCLANG;

type
  TCXEvalResultKind = Integer;

const
  CXEval_Int = 1 ;
  CXEval_Float = 2;
  CXEval_ObjCStrLiteral = 3;
  CXEval_StrLiteral = 4;
  CXEval_CFStr = 5;
  CXEval_Other = 6;
  CXEval_UnExposed = 0;

(**
 * Evaluation result of a cursor
 *)
type TCXEvalResult = Pointer;

(**
 * If cursor is a statement declaration tries to evaluate the
 * statement and if its variable, tries to evaluate its initializer,
 * into its corresponding type.
 *)
function clang_Cursor_Evaluate(C: TCXCursor): TCXEvalResult; cdecl external LIBCLANG;

(**
 * Returns the kind of the evaluated result.
 *)
function clang_EvalResult_getKind(E: TCXEvalResult): TCXEvalResultKind; cdecl external LIBCLANG;

(**
 * Returns the evaluation result as integer if the
 * kind is Int.
 *)
function clang_EvalResult_getAsInt(E: TCXEvalResult): Integer; cdecl external LIBCLANG;

(**
 * Returns the evaluation result as a long long integer if the
 * kind is Int. This prevents overflows that may happen if the result is
 * returned with clang_EvalResult_getAsInt.
 *)
function clang_EvalResult_getAsLongLong(E: TCXEvalResult): Int64; cdecl external LIBCLANG;

(**
 * Returns a non-zero value if the kind is Int and the evaluation
 * result resulted in an unsigned integer.
 *)
function clang_EvalResult_isUnsignedInt(E: TCXEvalResult): Cardinal; cdecl external LIBCLANG;

(**
 * Returns the evaluation result as an unsigned integer if
 * the kind is Int and clang_EvalResult_isUnsignedInt is non-zero.
 *)
function clang_EvalResult_getAsUnsigned(E: TCXEvalResult): UInt64; cdecl external LIBCLANG;

(**
 * Returns the evaluation result as double if the
 * kind is double.
 *)
function clang_EvalResult_getAsDouble(E: TCXEvalResult): Double; cdecl external LIBCLANG;

(**
 * Returns the evaluation result as a constant string if the
 * kind is other than Int or float. User must not free this pointer,
 * instead call clang_EvalResult_dispose on the CXEvalResult returned
 * by clang_Cursor_Evaluate.
 *)
function clang_EvalResult_getAsStr(E: TCXEvalResult): PAnsiChar; cdecl external LIBCLANG;

(**
 * Disposes the created Eval memory.
 *)
procedure clang_EvalResult_dispose(E: TCXEvalResult); cdecl external LIBCLANG;
(**
 * @}
 *)

(** \defgroup CINDEX_REMAPPING Remapping functions
 *
 * @{
 *)

(**
 * A remapping of original source files and their translated files.
 *)
type TCXRemapping = Pointer;

(**
 * Retrieve a remapping.
 *
 * \param path the path that contains metadata about remappings.
 *
 * \returns the requested remapping. This remapping must be freed
 * via a call to \c clang_remap_dispose(). Can return NULL if an error occurred.
 *)
function clang_getRemappings(const path: PAnsiChar): TCXRemapping; cdecl external LIBCLANG;

(**
 * Retrieve a remapping.
 *
 * \param filePaths pointer to an array of file paths containing remapping info.
 *
 * \param numFiles number of file paths.
 *
 * \returns the requested remapping. This remapping must be freed
 * via a call to \c clang_remap_dispose(). Can return NULL if an error occurred.
 *)
function clang_getRemappingsFromFileList(const filePaths: PPAnsiChar; numFiles: Cardinal): TCXRemapping; cdecl external LIBCLANG;

(**
 * Determine the number of remappings.
 *)
function clang_remap_getNumFiles(p1: TCXRemapping): Cardinal; cdecl external LIBCLANG;

(**
 * Get the original and the associated filename from the remapping.
 *
 * \param original If non-NULL, will be set to the original filename.
 *
 * \param transformed If non-NULL, will be set to the filename that the original
 * is associated with.
 *)
procedure clang_remap_getFilenames(p1: TCXRemapping; index: Cardinal; original: PCXString; transformed: PCXString); cdecl external LIBCLANG;

(**
 * Dispose the remapping.
 *)
procedure clang_remap_dispose(p1: TCXRemapping); cdecl external LIBCLANG;

(**
 * @}
 *)

(** \defgroup CINDEX_HIGH Higher level API functions
 *
 * @{
 *)

type
  TCXVisitorResult = Integer;

const
  CXVisit_Break = 0;
  CXVisit_Continue = CXVisit_Break + 1;

type
  TCXCursorAndRangeVisitor = record
    context: Pointer;
    visit: function(context: Pointer; p2: TCXCursor; p3: TCXSourceRange): TCXVisitorResult; cdecl;
  end;
  PCXCursorAndRangeVisitor = ^TCXCursorAndRangeVisitor;

type
  TCXResult = Integer;

const
  CXResult_Success = 0;
  CXResult_Invalid = 1;
  CXResult_VisitBreak = 2;

(**
 * Find references of a declaration in a specific file.
 *
 * \param cursor pointing to a declaration or a reference of one.
 *
 * \param file to search for references.
 *
 * \param visitor callback that will receive pairs of CXCursor/CXSourceRange for
 * each reference found.
 * The CXSourceRange will point inside the file; if the reference is inside
 * a macro (and not a macro argument) the CXSourceRange will be invalid.
 *
 * \returns one of the CXResult enumerators.
 *)
function clang_findReferencesInFile(cursor: TCXCursor; _file: TCXFile; visitor: TCXCursorAndRangeVisitor): TCXResult; cdecl external LIBCLANG;

(**
 * Find #import/#include directives in a specific file.
 *
 * \param TU translation unit containing the file to query.
 *
 * \param file to search for #import/#include directives.
 *
 * \param visitor callback that will receive pairs of CXCursor/CXSourceRange for
 * each directive found.
 *
 * \returns one of the CXResult enumerators.
 *)
function clang_findIncludesInFile(TU: TCXTranslationUnit; _file: TCXFile; visitor: TCXCursorAndRangeVisitor): TCXResult; cdecl external LIBCLANG;

(**
 * The client's data object that is associated with a CXFile.
 *)
type TCXIdxClientFile = Pointer;
type PCXIdxClientFile = ^TCXIdxClientFile;

(**
 * The client's data object that is associated with a semantic entity.
 *)
type TCXIdxClientEntity = Pointer;

(**
 * The client's data object that is associated with a semantic container
 * of entities.
 *)
type TCXIdxClientContainer = Pointer;

(**
 * The client's data object that is associated with an AST file (PCH
 * or module).
 *)
type TCXIdxClientASTFile = Pointer;

(**
 * Source location passed to index callbacks.
 *)
type
  TCXIdxLoc = record
    ptr_data: array [0..2-1] of Pointer;
    int_data: Cardinal;
  end;
  PCXIdxLoc = ^TCXIdxLoc;

(**
 * Data for ppIncludedFile callback.
 *)
type
  TCXIdxIncludedFileInfo = record
    hashLoc: TCXIdxLoc;
    filename: PAnsiChar;
    _file: TCXFile;
    isImport: Integer;
    isAngled: Integer;
    isModuleImport: Integer;
  end;
  PCXIdxIncludedFileInfo = ^TCXIdxIncludedFileInfo;

(**
 * Data for IndexerCallbacks#importedASTFile.
 *)
type
  TCXIdxImportedASTFileInfo = record
    _file: TCXFile;
    module: TCXModule;
    loc: TCXIdxLoc;
    isImplicit: Integer;
  end;
  PCXIdxImportedASTFileInfo = ^TCXIdxImportedASTFileInfo;

type
  TCXIdxEntityKind = Integer;

const
  CXIdxEntity_Unexposed = 0;
  CXIdxEntity_Typedef = 1;
  CXIdxEntity_Function = 2;
  CXIdxEntity_Variable = 3;
  CXIdxEntity_Field = 4;
  CXIdxEntity_EnumConstant = 5;
  CXIdxEntity_ObjCClass = 6;
  CXIdxEntity_ObjCProtocol = 7;
  CXIdxEntity_ObjCCategory = 8;
  CXIdxEntity_ObjCInstanceMethod = 9;
  CXIdxEntity_ObjCClassMethod = 10;
  CXIdxEntity_ObjCProperty = 11;
  CXIdxEntity_ObjCIvar = 12;
  CXIdxEntity_Enum = 13;
  CXIdxEntity_Struct = 14;
  CXIdxEntity_Union = 15;
  CXIdxEntity_CXXClass = 16;
  CXIdxEntity_CXXNamespace = 17;
  CXIdxEntity_CXXNamespaceAlias = 18;
  CXIdxEntity_CXXStaticVariable = 19;
  CXIdxEntity_CXXStaticMethod = 20;
  CXIdxEntity_CXXInstanceMethod = 21;
  CXIdxEntity_CXXConstructor = 22;
  CXIdxEntity_CXXDestructor = 23;
  CXIdxEntity_CXXConversionFunction = 24;
  CXIdxEntity_CXXTypeAlias = 25;
  CXIdxEntity_CXXInterface = 26;

type
  TCXIdxEntityLanguage = Integer;

const
  CXIdxEntityLang_None = 0;
  CXIdxEntityLang_C = 1;
  CXIdxEntityLang_ObjC = 2;
  CXIdxEntityLang_CXX = 3;
  CXIdxEntityLang_Swift = 4;

(**
 * Extra C++ template information for an entity. This can apply to:
 * CXIdxEntity_Function
 * CXIdxEntity_CXXClass
 * CXIdxEntity_CXXStaticMethod
 * CXIdxEntity_CXXInstanceMethod
 * CXIdxEntity_CXXConstructor
 * CXIdxEntity_CXXConversionFunction
 * CXIdxEntity_CXXTypeAlias
 *)
type
  TCXIdxEntityCXXTemplateKind = Integer;

const
  CXIdxEntity_NonTemplate = 0;
  CXIdxEntity_Template = 1;
  CXIdxEntity_TemplatePartialSpecialization = 2;
  CXIdxEntity_TemplateSpecialization = 3;

type
  TCXIdxAttrKind = Integer;

const
  CXIdxAttr_Unexposed = 0;
  CXIdxAttr_IBAction = 1;
  CXIdxAttr_IBOutlet = 2;
  CXIdxAttr_IBOutletCollection = 3;

type
  TCXIdxAttrInfo = record
    kind: TCXIdxAttrKind;
    cursor: TCXCursor;
    loc: TCXIdxLoc;
  end;
  PCXIdxAttrInfo = ^TCXIdxAttrInfo;
  PPCXIdxAttrInfo = ^PCXIdxAttrInfo;

type
  TCXIdxEntityInfo = record
    kind: TCXIdxEntityKind;
    templateKind: TCXIdxEntityCXXTemplateKind;
    lang: TCXIdxEntityLanguage;
    name: PAnsiChar;
    USR: PAnsiChar;
    cursor: TCXCursor;
    attributes: PPCXIdxAttrInfo;
    numAttributes: Cardinal;
  end;
  PCXIdxEntityInfo = ^TCXIdxEntityInfo;

type
  TCXIdxContainerInfo = record
    cursor: TCXCursor;
  end;
  PCXIdxContainerInfo = ^TCXIdxContainerInfo;

type
  TCXIdxIBOutletCollectionAttrInfo = record
    attrInfo: PCXIdxAttrInfo;
    objcClass: PCXIdxEntityInfo;
    classCursor: TCXCursor;
    classLoc: TCXIdxLoc;
  end;
  PCXIdxIBOutletCollectionAttrInfo = ^TCXIdxIBOutletCollectionAttrInfo;

type
  TCXIdxDeclInfoFlags = Integer;

const
  CXIdxDeclFlag_Skipped = $1;

type
  TCXIdxDeclInfo = record
    entityInfo: PCXIdxEntityInfo;
    cursor: TCXCursor;
    loc: TCXIdxLoc;
    semanticContainer: PCXIdxContainerInfo;
    lexicalContainer: PCXIdxContainerInfo;
    isRedeclaration: Integer;
    isDefinition: Integer;
    isContainer: Integer;
    declAsContainer: PCXIdxContainerInfo;
    isImplicit: Integer;
    attributes: PPCXIdxAttrInfo;
    numAttributes: Cardinal;
    flags: Cardinal;
  end;
  PCXIdxDeclInfo = ^TCXIdxDeclInfo;

type
  TCXIdxObjCContainerKind = Integer;

const
  CXIdxObjCContainer_ForwardRef = 0;
  CXIdxObjCContainer_Interface = 1;
  CXIdxObjCContainer_Implementation = 2;

type
  TCXIdxObjCContainerDeclInfo = record
    declInfo: PCXIdxDeclInfo;
    kind: TCXIdxObjCContainerKind;
  end;
  PCXIdxObjCContainerDeclInfo = ^TCXIdxObjCContainerDeclInfo;

type
  TCXIdxBaseClassInfo = record
    base: PCXIdxEntityInfo;
    cursor: TCXCursor;
    loc: TCXIdxLoc;
  end;
  PCXIdxBaseClassInfo = ^TCXIdxBaseClassInfo;
  PPCXIdxBaseClassInfo = ^PCXIdxBaseClassInfo;

type
  TCXIdxObjCProtocolRefInfo = record
    protocol: PCXIdxEntityInfo;
    cursor: TCXCursor;
    loc: TCXIdxLoc;
  end;
  PCXIdxObjCProtocolRefInfo = ^TCXIdxObjCProtocolRefInfo;
  PPCXIdxObjCProtocolRefInfo = ^PCXIdxObjCProtocolRefInfo;

type
  TCXIdxObjCProtocolRefListInfo = record
    protocols: PPCXIdxObjCProtocolRefInfo;
    numProtocols: Cardinal;
  end;
  PCXIdxObjCProtocolRefListInfo = ^TCXIdxObjCProtocolRefListInfo;

type
  TCXIdxObjCInterfaceDeclInfo = record
    containerInfo: PCXIdxObjCContainerDeclInfo;
    superInfo: PCXIdxBaseClassInfo;
    protocols: PCXIdxObjCProtocolRefListInfo;
  end;
  PCXIdxObjCInterfaceDeclInfo = ^TCXIdxObjCInterfaceDeclInfo;

type
  TCXIdxObjCCategoryDeclInfo = record
    containerInfo: PCXIdxObjCContainerDeclInfo;
    objcClass: PCXIdxEntityInfo;
    classCursor: TCXCursor;
    classLoc: TCXIdxLoc;
    protocols: PCXIdxObjCProtocolRefListInfo;
  end;
  PCXIdxObjCCategoryDeclInfo = ^TCXIdxObjCCategoryDeclInfo;

type
  TCXIdxObjCPropertyDeclInfo = record
    declInfo: PCXIdxDeclInfo;
    getter: PCXIdxEntityInfo;
    setter: PCXIdxEntityInfo;
  end;
  PCXIdxObjCPropertyDeclInfo = ^TCXIdxObjCPropertyDeclInfo;

type
  TCXIdxCXXClassDeclInfo = record
    declInfo: PCXIdxDeclInfo;
    bases: PPCXIdxBaseClassInfo;
    numBases: Cardinal;
  end;
  PCXIdxCXXClassDeclInfo = ^TCXIdxCXXClassDeclInfo;

(**
 * Data for IndexerCallbacks#indexEntityReference.
 *)
type
  TCXIdxEntityRefKind = Integer;

const
  CXIdxEntityRef_Direct = 1;
  CXIdxEntityRef_Implicit = 2;

(**
 * Roles that are attributed to symbol occurrences.
 *
 * Internal: this currently mirrors low 9 bits of clang::index::SymbolRole with
 * higher bits zeroed. These high bits may be exposed in the future.
 *)
type
  TCXSymbolRole = Integer;

const
  CXSymbolRole_None = 0;
  CXSymbolRole_Declaration = 1 shl 0;
  CXSymbolRole_Definition = 1 shl 1;
  CXSymbolRole_Reference = 1 shl 2;
  CXSymbolRole_Read = 1 shl 3;
  CXSymbolRole_Write = 1 shl 4;
  CXSymbolRole_Call = 1 shl 5;
  CXSymbolRole_Dynamic = 1 shl 6;
  CXSymbolRole_AddressOf = 1 shl 7;
  CXSymbolRole_Implicit = 1 shl 8;

(**
 * Data for IndexerCallbacks#indexEntityReference.
 *)
type
  TCXIdxEntityRefInfo = record
    kind: TCXIdxEntityRefKind;
    cursor: TCXCursor;
    loc: TCXIdxLoc;
    referencedEntity: PCXIdxEntityInfo;
    parentEntity: PCXIdxEntityInfo;
    container: PCXIdxContainerInfo;
    role: TCXSymbolRole;
  end;
  PCXIdxEntityRefInfo = ^TCXIdxEntityRefInfo;

(**
 * A group of callbacks used by #clang_indexSourceFile and
 * #clang_indexTranslationUnit.
 *)
type
  TIndexerCallbacks = record
    abortQuery: function(client_data: TCXClientData; reserved: Pointer): Integer; cdecl;
    diagnostic: procedure(client_data: TCXClientData; p2: TCXDiagnosticSet; reserved: Pointer); cdecl;
    enteredMainFile: function(client_data: TCXClientData; mainFile: TCXFile; reserved: Pointer): TCXIdxClientFile; cdecl;
    ppIncludedFile: function(client_data: TCXClientData; const p2: PCXIdxIncludedFileInfo): TCXIdxClientFile; cdecl;
    importedASTFile: function(client_data: TCXClientData; const p2: PCXIdxImportedASTFileInfo): TCXIdxClientASTFile; cdecl;
    startedTranslationUnit: function(client_data: TCXClientData; reserved: Pointer): TCXIdxClientContainer; cdecl;
    indexDeclaration: procedure(client_data: TCXClientData; const p2: PCXIdxDeclInfo); cdecl;
    indexEntityReference: procedure(client_data: TCXClientData; const p2: PCXIdxEntityRefInfo); cdecl;
  end;
  PIndexerCallbacks = ^TIndexerCallbacks;

function clang_index_isEntityObjCContainerKind(p1: TCXIdxEntityKind): Integer; cdecl external LIBCLANG;
function clang_index_getObjCContainerDeclInfo(const p1: PCXIdxDeclInfo): PCXIdxObjCContainerDeclInfo; cdecl external LIBCLANG;

function clang_index_getObjCInterfaceDeclInfo(const p1: PCXIdxDeclInfo): PCXIdxObjCInterfaceDeclInfo; cdecl external LIBCLANG;

function clang_index_getObjCCategoryDeclInfo(const p1: PCXIdxDeclInfo): PCXIdxObjCCategoryDeclInfo; cdecl external LIBCLANG;

function clang_index_getObjCProtocolRefListInfo(const p1: PCXIdxDeclInfo): PCXIdxObjCProtocolRefListInfo; cdecl external LIBCLANG;

function clang_index_getObjCPropertyDeclInfo(const p1: PCXIdxDeclInfo): PCXIdxObjCPropertyDeclInfo; cdecl external LIBCLANG;

function clang_index_getIBOutletCollectionAttrInfo(const p1: PCXIdxAttrInfo): PCXIdxIBOutletCollectionAttrInfo; cdecl external LIBCLANG;

function clang_index_getCXXClassDeclInfo(const p1: PCXIdxDeclInfo): PCXIdxCXXClassDeclInfo; cdecl external LIBCLANG;

(**
 * For retrieving a custom CXIdxClientContainer attached to a
 * container.
 *)
function clang_index_getClientContainer(const p1: PCXIdxContainerInfo): TCXIdxClientContainer; cdecl external LIBCLANG;

(**
 * For setting a custom CXIdxClientContainer attached to a
 * container.
 *)
procedure clang_index_setClientContainer(const p1: PCXIdxContainerInfo; p2: TCXIdxClientContainer); cdecl external LIBCLANG;

(**
 * For retrieving a custom CXIdxClientEntity attached to an entity.
 *)
function clang_index_getClientEntity(const p1: PCXIdxEntityInfo): TCXIdxClientEntity; cdecl external LIBCLANG;

(**
 * For setting a custom CXIdxClientEntity attached to an entity.
 *)
procedure clang_index_setClientEntity(const p1: PCXIdxEntityInfo; p2: TCXIdxClientEntity); cdecl external LIBCLANG;

(**
 * An indexing action/session, to be applied to one or multiple
 * translation units.
 *)
type TCXIndexAction = Pointer;

(**
 * An indexing action/session, to be applied to one or multiple
 * translation units.
 *
 * \param CIdx The index object with which the index action will be associated.
 *)
function clang_IndexAction_create(CIdx: TCXIndex): TCXIndexAction; cdecl external LIBCLANG;

(**
 * Destroy the given index action.
 *
 * The index action must not be destroyed until all of the translation units
 * created within that index action have been destroyed.
 *)
procedure clang_IndexAction_dispose(p1: TCXIndexAction); cdecl external LIBCLANG;

type
  TCXIndexOptFlags = Integer;

const
  CXIndexOpt_None = $0;
  CXIndexOpt_SuppressRedundantRefs = $1;
  CXIndexOpt_IndexFunctionLocalSymbols = $2;
  CXIndexOpt_IndexImplicitTemplateInstantiations = $4;
  CXIndexOpt_SuppressWarnings = $8;
  CXIndexOpt_SkipParsedBodiesInSession = $10;

(**
 * Index the given source file and the translation unit corresponding
 * to that file via callbacks implemented through #IndexerCallbacks.
 *
 * \param client_data pointer data supplied by the client, which will
 * be passed to the invoked callbacks.
 *
 * \param index_callbacks Pointer to indexing callbacks that the client
 * implements.
 *
 * \param index_callbacks_size Size of #IndexerCallbacks structure that gets
 * passed in index_callbacks.
 *
 * \param index_options A bitmask of options that affects how indexing is
 * performed. This should be a bitwise OR of the CXIndexOpt_XXX flags.
 *
 * \param[out] out_TU pointer to store a \c CXTranslationUnit that can be
 * reused after indexing is finished. Set to \c NULL if you do not require it.
 *
 * \returns 0 on success or if there were errors from which the compiler could
 * recover.  If there is a failure from which there is no recovery, returns
 * a non-zero \c CXErrorCode.
 *
 * The rest of the parameters are the same as #clang_parseTranslationUnit.
 *)
function clang_indexSourceFile(p1: TCXIndexAction; client_data: TCXClientData; index_callbacks: PIndexerCallbacks; index_callbacks_size: Cardinal; index_options: Cardinal; const source_filename: PAnsiChar; const command_line_args: PPAnsiChar; num_command_line_args: Integer; unsaved_files: PCXUnsavedFile; num_unsaved_files: Cardinal; out_TU: PCXTranslationUnit; TU_options: Cardinal): Integer; cdecl external LIBCLANG;

(**
 * Same as clang_indexSourceFile but requires a full command line
 * for \c command_line_args including argv[0]. This is useful if the standard
 * library paths are relative to the binary.
 *)
function clang_indexSourceFileFullArgv(p1: TCXIndexAction; client_data: TCXClientData; index_callbacks: PIndexerCallbacks; index_callbacks_size: Cardinal; index_options: Cardinal; const source_filename: PAnsiChar; const command_line_args: PPAnsiChar; num_command_line_args: Integer; unsaved_files: PCXUnsavedFile; num_unsaved_files: Cardinal; out_TU: PCXTranslationUnit; TU_options: Cardinal): Integer; cdecl external LIBCLANG;

(**
 * Index the given translation unit via callbacks implemented through
 * #IndexerCallbacks.
 *
 * The order of callback invocations is not guaranteed to be the same as
 * when indexing a source file. The high level order will be:
 *
 *   -Preprocessor callbacks invocations
 *   -Declaration/reference callbacks invocations
 *   -Diagnostic callback invocations
 *
 * The parameters are the same as #clang_indexSourceFile.
 *
 * \returns If there is a failure from which there is no recovery, returns
 * non-zero, otherwise returns 0.
 *)
function clang_indexTranslationUnit(p1: TCXIndexAction; client_data: TCXClientData; index_callbacks: PIndexerCallbacks; index_callbacks_size: Cardinal; index_options: Cardinal; p6: TCXTranslationUnit): Integer; cdecl external LIBCLANG;

(**
 * Retrieve the CXIdxFile, file, line, column, and offset represented by
 * the given CXIdxLoc.
 *
 * If the location refers into a macro expansion, retrieves the
 * location of the macro expansion and if it refers into a macro argument
 * retrieves the location of the argument.
 *)
procedure clang_indexLoc_getFileLocation(loc: TCXIdxLoc; indexFile: PCXIdxClientFile; _file: PCXFile; line: PCardinal; column: PCardinal; offset: PCardinal); cdecl external LIBCLANG;

(**
 * Retrieve the CXSourceLocation represented by the given CXIdxLoc.
 *)
function clang_indexLoc_getCXSourceLocation(loc: TCXIdxLoc): TCXSourceLocation; cdecl external LIBCLANG;

(**
 * Visitor invoked for each field found by a traversal.
 *
 * This visitor function will be invoked for each field found by
 * \c clang_Type_visitFields. Its first argument is the cursor being
 * visited, its second argument is the client data provided to
 * \c clang_Type_visitFields.
 *
 * The visitor should return one of the \c CXVisitorResult values
 * to direct \c clang_Type_visitFields.
 *)
type TCXFieldVisitor = function(C: TCXCursor; client_data: TCXClientData): TCXVisitorResult; cdecl;

(**
 * Visit the fields of a particular type.
 *
 * This function visits all the direct fields of the given cursor,
 * invoking the given \p visitor function with the cursors of each
 * visited field. The traversal may be ended prematurely, if
 * the visitor returns \c CXFieldVisit_Break.
 *
 * \param T the record type whose field may be visited.
 *
 * \param visitor the visitor function that will be invoked for each
 * field of \p T.
 *
 * \param client_data pointer data supplied by the client, which will
 * be passed to the visitor each time it is invoked.
 *
 * \returns a non-zero value if the traversal was terminated
 * prematurely by the visitor returning \c CXFieldVisit_Break.
 *)
function clang_Type_visitFields(T: TCXType; visitor: TCXFieldVisitor; client_data: TCXClientData): Cardinal; cdecl external LIBCLANG;
{$ENDREGION 'Index.h'}

{$REGION 'Documentation.h'}
(*==-- clang-c/Documentation.h - Utilities for comment processing -*- C -*-===*\
|*                                                                            *|
|* Part of the LLVM Project, under the Apache License v2.0 with LLVM          *|
|* Exceptions.                                                                *|
|* See https://llvm.org/LICENSE.txt for license information.                  *|
|* SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception                    *|
|*                                                                            *|
|*===----------------------------------------------------------------------===*|
|*                                                                            *|
|* This header provides a supplementary interface for inspecting              *|
|* documentation comments.                                                    *|
|*                                                                            *|
\*===----------------------------------------------------------------------===*)

(**
 * \defgroup CINDEX_COMMENT Comment introspection
 *
 * The routines in this group provide access to information in documentation
 * comments. These facilities are distinct from the core and may be subject to
 * their own schedule of stability and deprecation.
 *
 * @{
 *)

(**
 * A parsed comment.
 *)
type
  {$IFDEF WIN32}
  TCXComment = UInt64;
  {$ELSE}
  TCXComment = record
    ASTNode: Pointer;
    TranslationUnit: TCXTranslationUnit;
  end;
  {$ENDIF}
  PCXComment = ^TCXComment;

(**
 * Given a cursor that represents a documentable entity (e.g.,
 * declaration), return the associated parsed comment as a
 * \c CXComment_FullComment AST node.
 *)
function clang_Cursor_getParsedComment(C: TCXCursor): TCXComment; cdecl external LIBCLANG;

(**
 * Describes the type of the comment AST node (\c CXComment).  A comment
 * node can be considered block content (e. g., paragraph), inline content
 * (plain text) or neither (the root AST node).
 *)
type
  TCXCommentKind = Integer;

const
  CXComment_Null = 0;
  CXComment_Text = 1;
  CXComment_InlineCommand = 2;
  CXComment_HTMLStartTag = 3;
  CXComment_HTMLEndTag = 4;
  CXComment_Paragraph = 5;
  CXComment_BlockCommand = 6;
  CXComment_ParamCommand = 7;
  CXComment_TParamCommand = 8;
  CXComment_VerbatimBlockCommand = 9;
  CXComment_VerbatimBlockLine = 10;
  CXComment_VerbatimLine = 11;
  CXComment_FullComment = 12;

(**
 * The most appropriate rendering mode for an inline command, chosen on
 * command semantics in Doxygen.
 *)
type
  TCXCommentInlineCommandRenderKind = Integer;

const
  CXCommentInlineCommandRenderKind_Normal = 0;
  CXCommentInlineCommandRenderKind_Bold = CXCommentInlineCommandRenderKind_Normal + 1;
  CXCommentInlineCommandRenderKind_Monospaced = CXCommentInlineCommandRenderKind_Bold + 1;
  CXCommentInlineCommandRenderKind_Emphasized = CXCommentInlineCommandRenderKind_Monospaced + 1;

(**
 * Describes parameter passing direction for \\param or \\arg command.
 *)
type
  TCXCommentParamPassDirection = Integer;

const
  CXCommentParamPassDirection_In = 0;
  CXCommentParamPassDirection_Out = CXCommentParamPassDirection_In + 1;
  CXCommentParamPassDirection_InOut = CXCommentParamPassDirection_Out + 1;

(**
 * \param Comment AST node of any kind.
 *
 * \returns the type of the AST node.
 *)
function clang_Comment_getKind(Comment: TCXComment): TCXCommentKind; cdecl external LIBCLANG;

(**
 * \param Comment AST node of any kind.
 *
 * \returns number of children of the AST node.
 *)
function clang_Comment_getNumChildren(Comment: TCXComment): Cardinal; cdecl external LIBCLANG;

(**
 * \param Comment AST node of any kind.
 *
 * \param ChildIdx child index (zero-based).
 *
 * \returns the specified child of the AST node.
 *)
function clang_Comment_getChild(Comment: TCXComment; ChildIdx: Cardinal): TCXComment; cdecl external LIBCLANG;

(**
 * A \c CXComment_Paragraph node is considered whitespace if it contains
 * only \c CXComment_Text nodes that are empty or whitespace.
 *
 * Other AST nodes (except \c CXComment_Paragraph and \c CXComment_Text) are
 * never considered whitespace.
 *
 * \returns non-zero if \c Comment is whitespace.
 *)
function clang_Comment_isWhitespace(Comment: TCXComment): Cardinal; cdecl external LIBCLANG;

(**
 * \returns non-zero if \c Comment is inline content and has a newline
 * immediately following it in the comment text.  Newlines between paragraphs
 * do not count.
 *)
function clang_InlineContentComment_hasTrailingNewline(Comment: TCXComment): Cardinal; cdecl external LIBCLANG;

(**
 * \param Comment a \c CXComment_Text AST node.
 *
 * \returns text contained in the AST node.
 *)
function clang_TextComment_getText(Comment: TCXComment): TCXString; cdecl external LIBCLANG;

(**
 * \param Comment a \c CXComment_InlineCommand AST node.
 *
 * \returns name of the inline command.
 *)
function clang_InlineCommandComment_getCommandName(Comment: TCXComment): TCXString; cdecl external LIBCLANG;

(**
 * \param Comment a \c CXComment_InlineCommand AST node.
 *
 * \returns the most appropriate rendering mode, chosen on command
 * semantics in Doxygen.
 *)
function clang_InlineCommandComment_getRenderKind(Comment: TCXComment): TCXCommentInlineCommandRenderKind; cdecl external LIBCLANG;

(**
 * \param Comment a \c CXComment_InlineCommand AST node.
 *
 * \returns number of command arguments.
 *)
function clang_InlineCommandComment_getNumArgs(Comment: TCXComment): Cardinal; cdecl external LIBCLANG;

(**
 * \param Comment a \c CXComment_InlineCommand AST node.
 *
 * \param ArgIdx argument index (zero-based).
 *
 * \returns text of the specified argument.
 *)
function clang_InlineCommandComment_getArgText(Comment: TCXComment; ArgIdx: Cardinal): TCXString; cdecl external LIBCLANG;

(**
 * \param Comment a \c CXComment_HTMLStartTag or \c CXComment_HTMLEndTag AST
 * node.
 *
 * \returns HTML tag name.
 *)
function clang_HTMLTagComment_getTagName(Comment: TCXComment): TCXString; cdecl external LIBCLANG;

(**
 * \param Comment a \c CXComment_HTMLStartTag AST node.
 *
 * \returns non-zero if tag is self-closing (for example, &lt;br /&gt;).
 *)
function clang_HTMLStartTagComment_isSelfClosing(Comment: TCXComment): Cardinal; cdecl external LIBCLANG;

(**
 * \param Comment a \c CXComment_HTMLStartTag AST node.
 *
 * \returns number of attributes (name-value pairs) attached to the start tag.
 *)
function clang_HTMLStartTag_getNumAttrs(Comment: TCXComment): Cardinal; cdecl external LIBCLANG;

(**
 * \param Comment a \c CXComment_HTMLStartTag AST node.
 *
 * \param AttrIdx attribute index (zero-based).
 *
 * \returns name of the specified attribute.
 *)
function clang_HTMLStartTag_getAttrName(Comment: TCXComment; AttrIdx: Cardinal): TCXString; cdecl external LIBCLANG;

(**
 * \param Comment a \c CXComment_HTMLStartTag AST node.
 *
 * \param AttrIdx attribute index (zero-based).
 *
 * \returns value of the specified attribute.
 *)
function clang_HTMLStartTag_getAttrValue(Comment: TCXComment; AttrIdx: Cardinal): TCXString; cdecl external LIBCLANG;

(**
 * \param Comment a \c CXComment_BlockCommand AST node.
 *
 * \returns name of the block command.
 *)
function clang_BlockCommandComment_getCommandName(Comment: TCXComment): TCXString; cdecl external LIBCLANG;

(**
 * \param Comment a \c CXComment_BlockCommand AST node.
 *
 * \returns number of word-like arguments.
 *)
function clang_BlockCommandComment_getNumArgs(Comment: TCXComment): Cardinal; cdecl external LIBCLANG;

(**
 * \param Comment a \c CXComment_BlockCommand AST node.
 *
 * \param ArgIdx argument index (zero-based).
 *
 * \returns text of the specified word-like argument.
 *)
function clang_BlockCommandComment_getArgText(Comment: TCXComment; ArgIdx: Cardinal): TCXString; cdecl external LIBCLANG;

(**
 * \param Comment a \c CXComment_BlockCommand or
 * \c CXComment_VerbatimBlockCommand AST node.
 *
 * \returns paragraph argument of the block command.
 *)
function clang_BlockCommandComment_getParagraph(Comment: TCXComment): TCXComment; cdecl external LIBCLANG;

(**
 * \param Comment a \c CXComment_ParamCommand AST node.
 *
 * \returns parameter name.
 *)
function clang_ParamCommandComment_getParamName(Comment: TCXComment): TCXString; cdecl external LIBCLANG;

(**
 * \param Comment a \c CXComment_ParamCommand AST node.
 *
 * \returns non-zero if the parameter that this AST node represents was found
 * in the function prototype and \c clang_ParamCommandComment_getParamIndex
 * function will return a meaningful value.
 *)
function clang_ParamCommandComment_isParamIndexValid(Comment: TCXComment): Cardinal; cdecl external LIBCLANG;

(**
 * \param Comment a \c CXComment_ParamCommand AST node.
 *
 * \returns zero-based parameter index in function prototype.
 *)
function clang_ParamCommandComment_getParamIndex(Comment: TCXComment): Cardinal; cdecl external LIBCLANG;

(**
 * \param Comment a \c CXComment_ParamCommand AST node.
 *
 * \returns non-zero if parameter passing direction was specified explicitly in
 * the comment.
 *)
function clang_ParamCommandComment_isDirectionExplicit(Comment: TCXComment): Cardinal; cdecl external LIBCLANG;

(**
 * \param Comment a \c CXComment_ParamCommand AST node.
 *
 * \returns parameter passing direction.
 *)
function clang_ParamCommandComment_getDirection(Comment: TCXComment): TCXCommentParamPassDirection; cdecl external LIBCLANG;

(**
 * \param Comment a \c CXComment_TParamCommand AST node.
 *
 * \returns template parameter name.
 *)
function clang_TParamCommandComment_getParamName(Comment: TCXComment): TCXString; cdecl external LIBCLANG;

(**
 * \param Comment a \c CXComment_TParamCommand AST node.
 *
 * \returns non-zero if the parameter that this AST node represents was found
 * in the template parameter list and
 * \c clang_TParamCommandComment_getDepth and
 * \c clang_TParamCommandComment_getIndex functions will return a meaningful
 * value.
 *)
function clang_TParamCommandComment_isParamPositionValid(Comment: TCXComment): Cardinal; cdecl external LIBCLANG;

(**
 * \param Comment a \c CXComment_TParamCommand AST node.
 *
 * \returns zero-based nesting depth of this parameter in the template parameter list.
 *
 * For example,
 * \verbatim
 *     template<typename C, template<typename T> class TT>
 *     void test(TT<int> aaa);
 * \endverbatim
 * for C and TT nesting depth is 0,
 * for T nesting depth is 1.
 *)
function clang_TParamCommandComment_getDepth(Comment: TCXComment): Cardinal; cdecl external LIBCLANG;

(**
 * \param Comment a \c CXComment_TParamCommand AST node.
 *
 * \returns zero-based parameter index in the template parameter list at a
 * given nesting depth.
 *
 * For example,
 * \verbatim
 *     template<typename C, template<typename T> class TT>
 *     void test(TT<int> aaa);
 * \endverbatim
 * for C and TT nesting depth is 0, so we can ask for index at depth 0:
 * at depth 0 C's index is 0, TT's index is 1.
 *
 * For T nesting depth is 1, so we can ask for index at depth 0 and 1:
 * at depth 0 T's index is 1 (same as TT's),
 * at depth 1 T's index is 0.
 *)
function clang_TParamCommandComment_getIndex(Comment: TCXComment; Depth: Cardinal): Cardinal; cdecl external LIBCLANG;

(**
 * \param Comment a \c CXComment_VerbatimBlockLine AST node.
 *
 * \returns text contained in the AST node.
 *)
function clang_VerbatimBlockLineComment_getText(Comment: TCXComment): TCXString; cdecl external LIBCLANG;

(**
 * \param Comment a \c CXComment_VerbatimLine AST node.
 *
 * \returns text contained in the AST node.
 *)
function clang_VerbatimLineComment_getText(Comment: TCXComment): TCXString; cdecl external LIBCLANG;

(**
 * Convert an HTML tag AST node to string.
 *
 * \param Comment a \c CXComment_HTMLStartTag or \c CXComment_HTMLEndTag AST
 * node.
 *
 * \returns string containing an HTML tag.
 *)
function clang_HTMLTagComment_getAsString(Comment: TCXComment): TCXString; cdecl external LIBCLANG;

(**
 * Convert a given full parsed comment to an HTML fragment.
 *
 * Specific details of HTML layout are subject to change.  Don't try to parse
 * this HTML back into an AST, use other APIs instead.
 *
 * Currently the following CSS classes are used:
 * \li "para-brief" for \paragraph and equivalent commands;
 * \li "para-returns" for \\returns paragraph and equivalent commands;
 * \li "word-returns" for the "Returns" word in \\returns paragraph.
 *
 * Function argument documentation is rendered as a \<dl\> list with arguments
 * sorted in function prototype order.  CSS classes used:
 * \li "param-name-index-NUMBER" for parameter name (\<dt\>);
 * \li "param-descr-index-NUMBER" for parameter description (\<dd\>);
 * \li "param-name-index-invalid" and "param-descr-index-invalid" are used if
 * parameter index is invalid.
 *
 * Template parameter documentation is rendered as a \<dl\> list with
 * parameters sorted in template parameter list order.  CSS classes used:
 * \li "tparam-name-index-NUMBER" for parameter name (\<dt\>);
 * \li "tparam-descr-index-NUMBER" for parameter description (\<dd\>);
 * \li "tparam-name-index-other" and "tparam-descr-index-other" are used for
 * names inside template template parameters;
 * \li "tparam-name-index-invalid" and "tparam-descr-index-invalid" are used if
 * parameter position is invalid.
 *
 * \param Comment a \c CXComment_FullComment AST node.
 *
 * \returns string containing an HTML fragment.
 *)
function clang_FullComment_getAsHTML(Comment: TCXComment): TCXString; cdecl external LIBCLANG;

(**
 * Convert a given full parsed comment to an XML document.
 *
 * A Relax NG schema for the XML can be found in comment-xml-schema.rng file
 * inside clang source tree.
 *
 * \param Comment a \c CXComment_FullComment AST node.
 *
 * \returns string containing an XML document.
 *)
function clang_FullComment_getAsXML(Comment: TCXComment): TCXString; cdecl external LIBCLANG;
{$ENDREGION 'Documentation.h'}

implementation

end.
