unit Neslib.Clang;
{< Delphi wrappers for LibClang 6.0.0.
   The main entry point to LibClang is TIndex.Create. }

{$SCOPEDENUMS ON}

interface

uses
  System.SysUtils,
  Neslib.Clang.Api;

type
  { Error codes returned by libclang routines.
    TError.Success is the only error code indicating success. Other error codes,
    including not yet assigned non-zero values, indicate errors. }
  TError = (
    { No error }
    Success = CXError_Success,

    { A generic error code, no further details are available.

      Errors of this kind can get their own specific error codes in future
      libclang versions. }
    Failure = CXError_Failure,

    { libclang crashed while performing the requested operation. }
    Crashed = CXError_Crashed,

    { The function detected that the arguments violate the function contract. }
    InvalidArguments = CXError_InvalidArguments,

    { An AST deserialization error has occurred. }
    AstReadError = CXError_ASTReadError);

type
  TGlobalOption = (
    { Used to indicate that threads that libclang creates for indexing purposes
      should use background priority.  }
    ThreadBackgroundPriorityForIndexing,

    { Used to indicate that threads that libclang creates for editing purposes
      should use background priority. }
    ThreadBackgroundPriorityForEditing);
  TGlobalOptions = set of TGlobalOption;

  { Extends TGlobalOptions }
  TGlobalOptionsHelper = record helper for TGlobalOptions
  public const
    { Used to indicate that all threads that libclang creates should use
      background priority.  }
    ThreadBackgroundPriorityForAll = [
      TGlobalOption.ThreadBackgroundPriorityForIndexing,
      TGlobalOption.ThreadBackgroundPriorityForEditing];
  end;

type
  { Flags that control the creation of translation units. }
  TTranslationUnitFlag = (
    { Used to indicate that the parser should construct a "detailed"
      preprocessing record, including all macro definitions and instantiations.

      Constructing a detailed preprocessing record requires more memory and time
      to parse, since the information contained in the record is usually not
      retained. However, it can be useful for applications that require more
      detailed information about the behavior of the preprocessor.  }
    DetailedPreprocessingRecord,

    { Used to indicate that the translation unit is incomplete.

      When a translation unit is considered "incomplete", semantic analysis that
      is typically performed at the end of the translation unit will be
      suppressed. For example, this suppresses the completion of tentative
      declarations in C and of instantiation of implicitly-instantiation
      function templates in C++. This option is typically used when parsing a
      header with the intent of producing a precompiled header.  }
    Incomplete,

    { Used to indicate that the translation unit should be built with an
      implicit precompiled header for the preamble.

      An implicit precompiled header is used as an optimization when a
      particular translation unit is likely to be reparsed many times when the
      sources aren't changing that often. In this case, an implicit precompiled
      header will be built containing all of the initial includes at the top of
      the main file (what we refer to as the "preamble" of the file). In
      subsequent parses, if the preamble or the files in it have not changed,
      ITranslationUnit.Reparse will re-use the implicit precompiled header to
      improve parsing performance. }
    PrecompiledPreamble,

    { Used to indicate that the translation unit should cache some
      code-completion results with each reparse of the source file.

      Caching of code-completion results is a performance optimization that
      introduces some overhead to reparsing but improves the performance of
      code-completion operations. }
    CacheCompletionResults,

    { Used to indicate that the translation unit will be serialized with
      ITranslationUnit.Save.

      This option is typically used when parsing a header with the intent of
      producing a precompiled header.  }
    ForSerialization,

    { DEPRECATED: Enabled chained precompiled preambles in C++.

      Note: this is a *temporary* option that is available only while we are
      testing C++ precompiled preamble support. It is deprecated. }
    CXXChainedPCH,

    { Used to indicate that function/method bodies should be skipped while
      parsing.

      This option can be used to search for declarations/definitions while
      ignoring the usages.  }
    SkipFunctionBodies,

    { Used to indicate that brief documentation comments should be included into
      the set of code completions returned from this translation unit. }
    IncludeBriefCommentsInCodeCompletion,

    { Used to indicate that the precompiled preamble should be created on the
      first parse. Otherwise it will be created on the first reparse. This
      trades runtime on the first parse (serializing the preamble takes time)
      for reduced runtime on the second parse (can now reuse the preamble).  }
    CreatePreambleOnFirstParse,

    { Do not stop processing when fatal errors are encountered.

      When fatal errors are encountered while parsing a translation unit,
      semantic analysis is typically stopped early when compiling code. A common
      source for fatal errors are unresolvable include files. For the purposes
      of an IDE, this is undesirable behavior and as much information as
      possible should be reported. Use this flag to enable this behavior. }
    KeepGoing,

    { Sets the preprocessor in a mode for parsing a single file only. }
    SingleFileParse,

    { Used in combination with SkipFunctionBodies to constrain the skipping of
      function bodies to the preamble.

      The function bodies of the main file are not skipped. }
    LimitSkipFunctionBodiesToPreamble,

    { Used to indicate that attributed types should be included in TCXType. }
    IncludeAttributedTypes,

    { Used to indicate that implicit attributes should be visited. }
    VisitImplicitAttributes,

    { Used to indicate that non-errors from included files should be ignored.

      If set, TTranslationUnit.GetAllDiagnostics will not report e.g. warnings
      from included files anymore. This speeds up GetAllDiagnostics for the case
      where these warnings are not of interest, as for an IDE for example, which
      typically shows only the diagnostics in the main file. }
    IgnoreNonErrorsFromIncludedFiles);
  TTranslationUnitFlags = set of TTranslationUnitFlag;

type
  { Describes the kind of error that occurred (if any) in a call to
    ITranslationUnit.Save. }
  TSaveResult = (
    { Indicates that no error occurred while saving a translation unit. }
    OK = CXSaveError_None,

    { Indicates that an unknown error occurred while attempting to save the
      file.

      This error typically indicates that file I/O failed when attempting to
      write the file. }
    Unknown = CXSaveError_Unknown,

    { Indicates that errors during translation prevented this attempt to save
      the translation unit.

      Errors that prevent the translation unit from being saved can be extracted
      ITranslationUnit.Diagnostics. }
    TranslationErrors = CXSaveError_TranslationErrors,

    { Indicates that the translation unit to be saved was somehow invalid. }
    InvalidTranslationUnit = CXSaveError_InvalidTU);

type
  { Flags that can be passed to ITranslationUnit.CodeCompleteAt to modify its
    behavior. }
  TCodeCompleteFlag = (
    { Whether to include macros within the set of code completions returned. }
    IncludeMacros,

    { Whether to include code patterns for language constructs within the set of
      code completions, e.g., for loops. }
    IncludeCodePatterns,

    { Whether to include brief documentation within the set of code completions
      returned. }
    IncludeBriefComments,

    { Whether to speed up completion by omitting top- or namespace-level
      entities defined in the preamble. There's no guarantee any particular
      entity is omitted. This may be useful if the headers are indexed
      externally. }
    SkipPreamble,

    { Whether to include completions with small fix-its, e.g. change '.' to '->'
      on member access, etc. }
    IncludeCompletionsWithFixIts);
  TCodeCompleteFlags = set of TCodeCompleteFlag;

type
  { Visitors must return one of these values. }
  TVisitorResult = (
    { Stop the visitor process. }
    Break = CXVisit_Break,

    { Continue the visitor process. }
    Continue = CXVisit_Continue);

type
  { Result of a visitor function. }
  TVisitResult = (
    { Function returned successfully. }
    Success = CXResult_Success,

    { One of the parameters was invalid for the function. }
    Invalid = CXResult_Invalid,

    { The function was terminated by a callback (e.g. it returned
      TVisitorResult.Break). }
    VisitBreak = CXResult_VisitBreak);

type
  { Describes how the traversal of the children of a particular cursor should
    proceed after visiting a particular child cursor.

    A value of this enumeration type should be returned by each TCursorVisitor
    to indicate how TCursor.VisitChildren proceeds. }
  TChildVisitResult = (
    { Terminates the cursor traversal. }
    Break = CXChildVisit_Break,

    { Continues the cursor traversal with the next sibling of the cursor just
      visited, without visiting its children. }
    Continue = CXChildVisit_Continue,

    { Recursively traverse the children of this cursor, using the same visitor. }
    Recurse = CXChildVisit_Recurse);

type
  { Describes the kind of entity that a cursor refers to. }
  TCursorKind = (
    (* Declarations *)

    { A declaration whose specific kind is not exposed via this interface.

      Unexposed declarations have the same operations as any other kind of
      declaration; one can extract their location information, spelling, find
      their definitions, etc. However, the specific kind of the declaration is
      not reported. }
    UnexposedDecl = CXCursor_UnexposedDecl,

    { A C or C++ struct. }
    StructDecl = CXCursor_StructDecl,

    { A C or C++ union. }
    UnionDecl = CXCursor_UnionDecl,

    { A C++ class. }
    ClassDecl = CXCursor_ClassDecl,

    { An enumeration. }
    EnumDecl = CXCursor_EnumDecl,

    { A field (in C) or non-static data member (in C++) in a struct, union, or
      C++ class. }
    FieldDecl = CXCursor_FieldDecl,

    { An enumerator constant. }
    EnumConstantDecl = CXCursor_EnumConstantDecl,

    { A function. }
    FunctionDecl = CXCursor_FunctionDecl,

    { A variable. }
    VarDecl = CXCursor_VarDecl,

    { A function or method parameter. }
    ParmDecl = CXCursor_ParmDecl,

    { An Objective-C @@interface. }
    ObjCInterfaceDecl = CXCursor_ObjCInterfaceDecl,

    { An Objective-C @@interface for a category. }
    ObjCCategoryDecl = CXCursor_ObjCCategoryDecl,

    { An Objective-C @@protocol declaration. }
    ObjCProtocolDecl = CXCursor_ObjCProtocolDecl,

    { An Objective-C @@property declaration. }
    ObjCPropertyDecl = CXCursor_ObjCPropertyDecl,

    { An Objective-C instance variable. }
    ObjCIvarDecl = CXCursor_ObjCIvarDecl,

    { An Objective-C instance method. }
    ObjCInstanceMethodDecl = CXCursor_ObjCInstanceMethodDecl,

    { An Objective-C class method. }
    ObjCClassMethodDecl = CXCursor_ObjCClassMethodDecl,

    { An Objective-C @@implementation. }
    ObjCImplementationDecl = CXCursor_ObjCImplementationDecl,

    { An Objective-C @@implementation for a category. }
    ObjCCategoryImplDecl = CXCursor_ObjCCategoryImplDecl,

    { A typedef. }
    TypedefDecl = CXCursor_TypedefDecl,

    { A C++ class method. }
    CXXMethod = CXCursor_CXXMethod,

    { A C++ namespace. }
    Namespace = CXCursor_Namespace,

    { A linkage specification, e.g. 'extern "C"'. }
    LinkageSpec = CXCursor_LinkageSpec,

    { A C++ constructor. }
    Ctor = CXCursor_Constructor,

    { A C++ destructor. }
    Dtor = CXCursor_Destructor,

    { A C++ conversion function. }
    ConversionFunction = CXCursor_ConversionFunction,

    { A C++ template type parameter. }
    TemplateTypeParameter = CXCursor_TemplateTypeParameter,

    { A C++ non-type template parameter. }
    NonTypeTemplateParameter = CXCursor_NonTypeTemplateParameter,

    { A C++ template template parameter. }
    TemplateTemplateParameter = CXCursor_TemplateTemplateParameter,

    { A C++ function template. }
    FunctionTemplate = CXCursor_FunctionTemplate,

    { A C++ class template. }
    ClassTemplate = CXCursor_ClassTemplate,

    { A C++ class template partial specialization. }
    ClassTemplatePartialSpecialization = CXCursor_ClassTemplatePartialSpecialization,

    { A C++ namespace alias declaration. }
    NamespaceAlias = CXCursor_NamespaceAlias,

    { A C++ using directive. }
    UsingDirective = CXCursor_UsingDirective,

    { A C++ using declaration. }
    UsingDeclaration = CXCursor_UsingDeclaration,

    { A C++ alias declaration }
    TypeAliasDecl = CXCursor_TypeAliasDecl,

    { An Objective-C @@synthesize definition. }
    ObjCSynthesizeDecl = CXCursor_ObjCSynthesizeDecl,

    { An Objective-C @@dynamic definition. }
    ObjCDynamicDecl = CXCursor_ObjCDynamicDecl,

    { An access specifier. }
    CXXAccessSpecifier = CXCursor_CXXAccessSpecifier,

    FirstDecl = CXCursor_FirstDecl,
    LastDecl = CXCursor_LastDecl,

    (* References *)
    FirstRef = CXCursor_FirstRef,
    ObjCSuperClassRef = CXCursor_ObjCSuperClassRef,
    ObjCProtocolRef = CXCursor_ObjCProtocolRef,
    ObjCClassRef = CXCursor_ObjCClassRef,

    { A reference to a type declaration.

      A type reference occurs anywhere where a type is named but not declared.
      For example, given:

      @preformatted(
        typedef unsigned size_type;
        size_type size;
      )

      The typedef is a declaration of size_type (TypedefDecl), while the type of
      the variable "size" is referenced. The cursor referenced by the type of
      size is the typedef for size_type. }
    TypeRef = CXCursor_TypeRef,
    CXXBaseSpecifier = CXCursor_CXXBaseSpecifier,

    { A reference to a class template, function template, template parameter, or
      class template partial specialization. }
    TemplateRef = CXCursor_TemplateRef,

    { A reference to a namespace or namespace alias. }
    NamespaceRef = CXCursor_NamespaceRef,

    { A reference to a member of a struct, union, or class that occurs in some
      non-expression context, e.g., a designated initializer. }
    MemberRef = CXCursor_MemberRef,

    { A reference to a labeled statement.

      This cursor kind is used to describe the jump to "start_over" in the goto
      statement in the following example:

      @preformatted(
        start_over:
          ++counter;

          goto start_over;
      )

      A label reference cursor refers to a label statement. }
    LabelRef = CXCursor_LabelRef,

    (*A reference to a set of overloaded functions or function templates that
      has not yet been resolved to a specific function or function template.

      An overloaded declaration reference cursor occurs in C++ templates where
      a dependent name refers to a function. For example:

      @preformatted(
        template<typename T> void swap(T&, T&);

        struct X { ... };
        void swap(X&, X&);

        template<typename T>
        void reverse(T* first, T* last) {
          while (first < last - 1) {
            swap(*first, *--last);
            ++first;
          }
        }

        struct Y { };
        void swap(Y&, Y&);
      )

      Here, the identifier "swap" is associated with an overloaded declaration
      reference. In the template definition, "swap" refers to either of the two
      "swap" functions declared above, so both results will be available. At
      instantiation time, "swap" may also refer to other functions found via
      argument-dependent lookup (e.g., the "swap" function at the end of the
      example).

      The property TCursor.OverloadedDecls can be used to retrieve the
      definitions referenced by this cursor. *)
    OverloadedDeclRef = CXCursor_OverloadedDeclRef,

    { A reference to a variable that occurs in some non-expression context,
      e.g., a C++ lambda capture list. }
    VariableRef = CXCursor_VariableRef,

    LastRef = CXCursor_LastRef,

    (* Error conditions *)
    FirstInvalid = CXCursor_FirstInvalid,
    InvalidFile = CXCursor_InvalidFile,
    NoDeclFound = CXCursor_NoDeclFound,
    NotImplemented = CXCursor_NotImplemented,
    InvalidCode = CXCursor_InvalidCode,
    LastInvalid = CXCursor_LastInvalid,

    (* Expressions *)
    FirstExpr = CXCursor_FirstExpr,

    { An expression whose specific kind is not exposed via this interface.

      Unexposed expressions have the same operations as any other kind of
      expression; one can extract their location information, spelling,
      children, etc. However, the specific kind of the expression is not
      reported. }
    UnexposedExpr = CXCursor_UnexposedExpr,

    { An expression that refers to some value declaration, such as a function,
      variable, or enumerator. }
    DeclRefExpr = CXCursor_DeclRefExpr,

    { An expression that refers to a member of a struct, union, class,
      Objective-C class, etc. }
    MemberRefExpr = CXCursor_MemberRefExpr,

    { An expression that calls a function. }
    CallExpr = CXCursor_CallExpr,

    { An expression that sends a message to an Objective-C object or class. }
    ObjCMessageExpr = CXCursor_ObjCMessageExpr,

    { An expression that represents a block literal. }
    BlockExpr = CXCursor_BlockExpr,

    { An integer literal. }
    IntegerLiteral = CXCursor_IntegerLiteral,

    { A floating point number literal. }
    FloatingLiteral = CXCursor_FloatingLiteral,

    { An imaginary number literal. }
    ImaginaryLiteral = CXCursor_ImaginaryLiteral,

    { A string literal. }
    StringLiteral = CXCursor_StringLiteral,

    { A character literal. }
    CharacterLiteral = CXCursor_CharacterLiteral,

    { A parenthesized expression, e.g. "(1)".
      This AST node is only formed if full location information is requested. }
    ParenExpr = CXCursor_ParenExpr,

    { This represents the unary-expression's (except sizeof and alignof). }
    UnaryOperator = CXCursor_UnaryOperator,

    { [C99 6.5.2.1] Array Subscripting. }
    ArraySubscriptExpr = CXCursor_ArraySubscriptExpr,

    { A builtin binary operation expression such as "x + y" or "x <= y". }
    BinaryOperator = CXCursor_BinaryOperator,

    { Compound assignment such as "+=". }
    CompoundAssignOperator = CXCursor_CompoundAssignOperator,

    { The ?: ternary operator. }
    ConditionalOperator = CXCursor_ConditionalOperator,

    { An explicit cast in C (C99 6.5.4) or a C-style cast in C++
      (C++ [expr.cast]), which uses the syntax (Type)expr.
      For example: (int)f. }
    CStyleCastExpr = CXCursor_CStyleCastExpr,

    { [C99 6.5.2.5] }
    CompoundLiteralExpr = CXCursor_CompoundLiteralExpr,

    { Describes an C or C++ initializer list. }
    InitListExpr = CXCursor_InitListExpr,

    { The GNU address of label extension, representing &&label. }
    AddrLabelExpr = CXCursor_AddrLabelExpr,

    (* This is the GNU Statement Expression extension: ({int X=4; X;}) *)
    StmtExpr = CXCursor_StmtExpr,

    { Represents a C11 generic selection. }
    GenericSelectionExpr = CXCursor_GenericSelectionExpr,

    { Implements the GNU __null extension, which is a name for a null pointer
      constant that has integral type (e.g., int or long) and is the same size
      and alignment as a pointer.

      The __null extension is typically only used by system headers, which
      define NULL as __null in C++ rather than using 0 (which is an integer that
      may not match the size of a pointer). }
    GNUNullExpr = CXCursor_GNUNullExpr,

    { C++'s static_cast<> expression. }
    CXXStaticCastExpr = CXCursor_CXXStaticCastExpr,

    { C++'s dynamic_cast<> expression. }
    CXXDynamicCastExpr = CXCursor_CXXDynamicCastExpr,

    { C++'s reinterpret_cast<> expression. }
    CXXReinterpretCastExpr = CXCursor_CXXReinterpretCastExpr,

    { C++'s const_cast<> expression. }
    CXXConstCastExpr = CXCursor_CXXConstCastExpr,

    { Represents an explicit C++ type conversion that uses "functional" notion
      (C++ [expr.type.conv]).
      Example: x = int(0.5); }
    CXXFunctionalCastExpr = CXCursor_CXXFunctionalCastExpr,

    { A C++ typeid expression (C++ [expr.typeid]). }
    CXXTypeidExpr = CXCursor_CXXTypeidExpr,

    { [C++ 2.13.5] C++ Boolean Literal. }
    CXXBoolLiteralExpr = CXCursor_CXXBoolLiteralExpr,

    { [C++0x 2.14.7] C++ Pointer Literal. }
    CXXNullPtrLiteralExpr = CXCursor_CXXNullPtrLiteralExpr,

    { Represents the "this" expression in C++ }
    CXXThisExpr = CXCursor_CXXThisExpr,

    { [C++ 15] C++ Throw Expression.
      This handles 'throw' and 'throw' assignment-expression. When
      assignment-expression isn't present, Op will be null. }
    CXXThrowExpr = CXCursor_CXXThrowExpr,

    { A new expression for memory allocation and constructor calls, e.g:
      "new CXXNewExpr(foo)". }
    CXXNewExpr = CXCursor_CXXNewExpr,

    { A delete expression for memory deallocation and destructor calls,
      e.g. "delete[] pArray". }
    CXXDeleteExpr = CXCursor_CXXDeleteExpr,

    { A unary expression. (noexcept, sizeof, or other traits) }
    UnaryExpr = CXCursor_UnaryExpr,

    { An Objective-C string literal i.e. @@"foo". }
    ObjCStringLiteral = CXCursor_ObjCStringLiteral,

    { An Objective-C @@encode expression. }
    ObjCEncodeExpr = CXCursor_ObjCEncodeExpr,

    { An Objective-C @@selector expression. }
    ObjCSelectorExpr = CXCursor_ObjCSelectorExpr,

    { An Objective-C @@protocol expression. }
    ObjCProtocolExpr = CXCursor_ObjCProtocolExpr,

    { An Objective-C "bridged" cast expression, which casts between Objective-C
      pointers and C pointers, transferring ownership in the process.
        NSString *str = (__bridge_transfer NSString *)CFCreateString(); }
    ObjCBridgedCastExpr = CXCursor_ObjCBridgedCastExpr,

    (*Represents a C++0x pack expansion that produces a sequence of expressions.

      A pack expansion expression contains a pattern (which itself is an
      expression) followed by an ellipsis. For example:

      @preformatted(
        template<typename F, typename ...Types>
        void forward(F f, Types &&...args) {
          f(static_cast<Types&&>(args)...);
        };
      ) *)
    PackExpansionExpr = CXCursor_PackExpansionExpr,

    (*Represents an expression that computes the length of a parameter pack.

        template<typename ...Types>
        struct count {
          static const unsigned value = sizeof...(Types);
        }; *)
    SizeOfPackExpr = CXCursor_SizeOfPackExpr,

    (*Represents a C++ lambda expression that produces a local function object.

        void abssort(float *x, unsigned N) {
          std::sort(x, x + N,
                    [](float a, float b) {
                      return std::abs(a) < std::abs(b);
                    });
        } *)
    LambdaExpr = CXCursor_LambdaExpr,

    { Objective-c Boolean Literal. }
    ObjCBoolLiteralExpr = CXCursor_ObjCBoolLiteralExpr,

    { Represents the "self" expression in an Objective-C method. }
    ObjCSelfExpr = CXCursor_ObjCSelfExpr,

    { OpenMP 4.0 [2.4, Array Section]. }
    OMPArraySectionExpr = CXCursor_OMPArraySectionExpr,

    { Represents an @@available(...) check. }
    ObjCAvailabilityCheckExpr = CXCursor_ObjCAvailabilityCheckExpr,

    { Fixed point literal }
    FixedPointLiteral = CXCursor_FixedPointLiteral,

    LastExpr = CXCursor_LastExpr,

    (* Statements *)
    FirstStmt = CXCursor_FirstStmt,

    { A statement whose specific kind is not exposed via this interface.

      Unexposed statements have the same operations as any other kind of
      statement; one can extract their location information, spelling, children,
      etc. However, the specific kind of the statement is not reported. }
    UnexposedStmt = CXCursor_UnexposedStmt,

    { A labelled statement in a function.

      This cursor kind is used to describe the "start_over:" label statement in
      the following example:

      @preformatted(
        start_over:
          ++counter;
      ) }
    LabelStmt = CXCursor_LabelStmt,

    (*A group of statements like { stmt stmt }.

      This cursor kind is used to describe compound statements, e.g. function
      bodies. *)
    CompoundStmt = CXCursor_CompoundStmt,

    { A case statement. }
    CaseStmt = CXCursor_CaseStmt,

    { A default statement. }
    DefaultStmt = CXCursor_DefaultStmt,

    { An if statement }
    IfStmt = CXCursor_IfStmt,

    { A switch statement. }
    SwitchStmt = CXCursor_SwitchStmt,

    { A while statement. }
    WhileStmt = CXCursor_WhileStmt,

    { A do statement. }
    DoStmt = CXCursor_DoStmt,

    { A for statement. }
    ForStmt = CXCursor_ForStmt,

    { A goto statement. }
    GotoStmt = CXCursor_GotoStmt,

    { An indirect goto statement. }
    IndirectGotoStmt = CXCursor_IndirectGotoStmt,

    { A continue statement. }
    ContinueStmt = CXCursor_ContinueStmt,

    { A break statement. }
    BreakStmt = CXCursor_BreakStmt,

    { A return statement. }
    ReturnStmt = CXCursor_ReturnStmt,

    { A GCC inline assembly statement extension. }
    GCCAsmStmt = CXCursor_GCCAsmStmt,
    AsmStmt = CXCursor_AsmStmt,

    { Objective-C's overall @@try-@@catch-@@finally statement. }
    ObjCAtTryStmt = CXCursor_ObjCAtTryStmt,

    { Objective-C's @@catch statement. }
    ObjCAtCatchStmt = CXCursor_ObjCAtCatchStmt,

    { Objective-C's @@finally statement. }
    ObjCAtFinallyStmt = CXCursor_ObjCAtFinallyStmt,

    { Objective-C's @@throw statement. }
    ObjCAtThrowStmt = CXCursor_ObjCAtThrowStmt,

    { Objective-C's @@synchronized statement. }
    ObjCAtSynchronizedStmt = CXCursor_ObjCAtSynchronizedStmt,

    { Objective-C's autorelease pool statement. }
    ObjCAutoreleasePoolStmt = CXCursor_ObjCAutoreleasePoolStmt,

    { Objective-C's collection statement. }
    ObjCForCollectionStmt = CXCursor_ObjCForCollectionStmt,

    { C++'s catch statement. }
    CXXCatchStmt = CXCursor_CXXCatchStmt,

    { C++'s try statement. }
    CXXTryStmt = CXCursor_CXXTryStmt,

    { C++'s for (* : *) statement. }
    CXXForRangeStmt = CXCursor_CXXForRangeStmt,

    { Windows Structured Exception Handling's try statement. }
    SEHTryStmt = CXCursor_SEHTryStmt,

    { Windows Structured Exception Handling's except statement. }
    SEHExceptStmt = CXCursor_SEHExceptStmt,

    { Windows Structured Exception Handling's finally statement. }
    SEHFinallyStmt = CXCursor_SEHFinallyStmt,

    { A MS inline assembly statement extension. }
    MSAsmStmt = CXCursor_MSAsmStmt,

    { The null statement ";": C99 6.8.3p3.
      This cursor kind is used to describe the null statement. }
    NullStmt = CXCursor_NullStmt,

    { Adaptor class for mixing declarations with statements and expressions. }
    DeclStmt = CXCursor_DeclStmt,

    { OpenMP parallel directive. }
    OMPParallelDirective = CXCursor_OMPParallelDirective,

    { OpenMP SIMD directive. }
    OMPSimdDirective = CXCursor_OMPSimdDirective,

    { OpenMP for directive. }
    OMPForDirective = CXCursor_OMPForDirective,

    { OpenMP sections directive. }
    OMPSectionsDirective = CXCursor_OMPSectionsDirective,

    { OpenMP section directive. }
    OMPSectionDirective = CXCursor_OMPSectionDirective,

    { OpenMP single directive. }
    OMPSingleDirective = CXCursor_OMPSingleDirective,

    { OpenMP parallel for directive. }
    OMPParallelForDirective = CXCursor_OMPParallelForDirective,

    { OpenMP parallel sections directive. }
    OMPParallelSectionsDirective = CXCursor_OMPParallelSectionsDirective,

    { OpenMP task directive. }
    OMPTaskDirective = CXCursor_OMPTaskDirective,

    { OpenMP master directive. }
    OMPMasterDirective = CXCursor_OMPMasterDirective,

    { OpenMP critical directive. }
    OMPCriticalDirective = CXCursor_OMPCriticalDirective,

    { OpenMP taskyield directive. }
    OMPTaskyieldDirective = CXCursor_OMPTaskyieldDirective,

    { OpenMP barrier directive. }
    OMPBarrierDirective = CXCursor_OMPBarrierDirective,

    { OpenMP taskwait directive. }
    OMPTaskwaitDirective = CXCursor_OMPTaskwaitDirective,

    { OpenMP flush directive. }
    OMPFlushDirective = CXCursor_OMPFlushDirective,

    { Windows Structured Exception Handling's leave statement. }
    SEHLeaveStmt = CXCursor_SEHLeaveStmt,

    { OpenMP ordered directive. }
    OMPOrderedDirective = CXCursor_OMPOrderedDirective,

    { OpenMP atomic directive. }
    OMPAtomicDirective = CXCursor_OMPAtomicDirective,

    { OpenMP for SIMD directive. }
    OMPForSimdDirective = CXCursor_OMPForSimdDirective,

    { OpenMP parallel for SIMD directive. }
    OMPParallelForSimdDirective = CXCursor_OMPParallelForSimdDirective,

    { OpenMP target directive. }
    OMPTargetDirective = CXCursor_OMPTargetDirective,

    { OpenMP teams directive. }
    OMPTeamsDirective = CXCursor_OMPTeamsDirective,

    { OpenMP taskgroup directive. }
    OMPTaskgroupDirective = CXCursor_OMPTaskgroupDirective,

    { OpenMP cancellation point directive. }
    OMPCancellationPointDirective = CXCursor_OMPCancellationPointDirective,

    { OpenMP cancel directive. }
    OMPCancelDirective = CXCursor_OMPCancelDirective,

    { OpenMP target data directive. }
    OMPTargetDataDirective = CXCursor_OMPTargetDataDirective,

    { OpenMP taskloop directive. }
    OMPTaskLoopDirective = CXCursor_OMPTaskLoopDirective,

    { OpenMP taskloop simd directive. }
    OMPTaskLoopSimdDirective = CXCursor_OMPTaskLoopSimdDirective,

    { OpenMP distribute directive. }
    OMPDistributeDirective = CXCursor_OMPDistributeDirective,

    { OpenMP target enter data directive. }
    OMPTargetEnterDataDirective = CXCursor_OMPTargetEnterDataDirective,

    { OpenMP target exit data directive. }
    OMPTargetExitDataDirective = CXCursor_OMPTargetExitDataDirective,

    { OpenMP target parallel directive. }
    OMPTargetParallelDirective = CXCursor_OMPTargetParallelDirective,

    { OpenMP target parallel for directive. }
    OMPTargetParallelForDirective = CXCursor_OMPTargetParallelForDirective,

    { OpenMP target update directive. }
    OMPTargetUpdateDirective = CXCursor_OMPTargetUpdateDirective,

    { OpenMP distribute parallel for directive. }
    OMPDistributeParallelForDirective = CXCursor_OMPDistributeParallelForDirective,

    { OpenMP distribute parallel for simd directive. }
    OMPDistributeParallelForSimdDirective = CXCursor_OMPDistributeParallelForSimdDirective,

    { OpenMP distribute simd directive. }
    OMPDistributeSimdDirective = CXCursor_OMPDistributeSimdDirective,

    { OpenMP target parallel for simd directive. }
    OMPTargetParallelForSimdDirective = CXCursor_OMPTargetParallelForSimdDirective,

    { OpenMP target simd directive. }
    OMPTargetSimdDirective = CXCursor_OMPTargetSimdDirective,

    { OpenMP teams distribute directive. }
    OMPTeamsDistributeDirective = CXCursor_OMPTeamsDistributeDirective,

    { OpenMP teams distribute simd directive. }
    OMPTeamsDistributeSimdDirective = CXCursor_OMPTeamsDistributeSimdDirective,

    { OpenMP teams distribute parallel for simd directive. }
    OMPTeamsDistributeParallelForSimdDirective = CXCursor_OMPTeamsDistributeParallelForSimdDirective,

    { OpenMP teams distribute parallel for directive. }
    OMPTeamsDistributeParallelForDirective = CXCursor_OMPTeamsDistributeParallelForDirective,

    { OpenMP target teams directive. }
    OMPTargetTeamsDirective = CXCursor_OMPTargetTeamsDirective,

    { OpenMP target teams distribute directive. }
    OMPTargetTeamsDistributeDirective = CXCursor_OMPTargetTeamsDistributeDirective,

    { OpenMP target teams distribute parallel for directive. }
    OMPTargetTeamsDistributeParallelForDirective = CXCursor_OMPTargetTeamsDistributeParallelForDirective,

    { OpenMP target teams distribute parallel for simd directive. }
    OMPTargetTeamsDistributeParallelForSimdDirective = CXCursor_OMPTargetTeamsDistributeParallelForSimdDirective,

    { OpenMP target teams distribute simd directive. }
    OMPTargetTeamsDistributeSimdDirective = CXCursor_OMPTargetTeamsDistributeSimdDirective,

    { C++2a std::bit_cast expression. }
    BuiltinBitCastExpr = CXCursor_BuiltinBitCastExpr,

    LastStmt = CXCursor_LastStmt,

    { Cursor that represents the translation unit itself.

      The translation unit cursor exists primarily to act as the root cursor for
      traversing the contents of a translation unit. }
    TranslationUnit = CXCursor_TranslationUnit,

    (* Attributes *)
    FirstAttr = CXCursor_FirstAttr,

    { An attribute whose specific kind is not exposed via this interface. }
    UnexposedAttr = CXCursor_UnexposedAttr,

    IBActionAttr = CXCursor_IBActionAttr,
    IBOutletAttr = CXCursor_IBOutletAttr,
    IBOutletCollectionAttr = CXCursor_IBOutletCollectionAttr,
    CXXFinalAttr = CXCursor_CXXFinalAttr,
    CXXOverrideAttr = CXCursor_CXXOverrideAttr,
    AnnotateAttr = CXCursor_AnnotateAttr,
    AsmLabelAttr = CXCursor_AsmLabelAttr,
    PackedAttr = CXCursor_PackedAttr,
    PureAttr = CXCursor_PureAttr,
    ConstAttr = CXCursor_ConstAttr,
    NoDuplicateAttr = CXCursor_NoDuplicateAttr,
    CUDAConstantAttr = CXCursor_CUDAConstantAttr,
    CUDADeviceAttr = CXCursor_CUDADeviceAttr,
    CUDAGlobalAttr = CXCursor_CUDAGlobalAttr,
    CUDAHostAttr = CXCursor_CUDAHostAttr,
    CUDASharedAttr = CXCursor_CUDASharedAttr,
    VisibilityAttr = CXCursor_VisibilityAttr,
    DLLExport = CXCursor_DLLExport,
    DLLImport = CXCursor_DLLImport,
    NSReturnsRetained = CXCursor_NSReturnsRetained,
    NSReturnsNotRetained = CXCursor_NSReturnsNotRetained,
    NSReturnsAutoreleased = CXCursor_NSReturnsAutoreleased,
    NSConsumesSelf = CXCursor_NSConsumesSelf,
    NSConsumed = CXCursor_NSConsumed,
    ObjCException = CXCursor_ObjCException,
    ObjCNSObject = CXCursor_ObjCNSObject,
    ObjCIndependentClass = CXCursor_ObjCIndependentClass,
    ObjCPreciseLifetime = CXCursor_ObjCPreciseLifetime,
    ObjCReturnsInnerPointer = CXCursor_ObjCReturnsInnerPointer,
    ObjCRequiresSuper = CXCursor_ObjCRequiresSuper,
    ObjCRootClass = CXCursor_ObjCRootClass,
    ObjCSubclassingRestricted = CXCursor_ObjCSubclassingRestricted,
    ObjCExplicitProtocolImpl = CXCursor_ObjCExplicitProtocolImpl,
    ObjCDesignatedInitializer = CXCursor_ObjCDesignatedInitializer,
    ObjCRuntimeVisible = CXCursor_ObjCRuntimeVisible,
    ObjCBoxable = CXCursor_ObjCBoxable,
    FlagEnum = CXCursor_FlagEnum,
    ConvergentAttr = CXCursor_ConvergentAttr,
    WarnUnusedAttr = CXCursor_WarnUnusedAttr,
    WarnUnusedResultAttr = CXCursor_WarnUnusedResultAttr,
    AlignedAttr = CXCursor_AlignedAttr,
    LastAttr = CXCursor_LastAttr,

    (* Preprocessing *)
    PreprocessingDirective = CXCursor_PreprocessingDirective,
    MacroDefinition = CXCursor_MacroDefinition,
    MacroExpansion = CXCursor_MacroExpansion,
    MacroInstantiation = CXCursor_MacroInstantiation,
    InclusionDirective = CXCursor_InclusionDirective,
    FirstPreprocessing = CXCursor_FirstPreprocessing,
    LastPreprocessing = CXCursor_LastPreprocessing,

    (* Extra Declarations *)

    { A module import declaration. }
    ModuleImportDecl = CXCursor_ModuleImportDecl,
    TypeAliasTemplateDecl = CXCursor_TypeAliasTemplateDecl,

    { A static_assert or _Static_assert node }
    StaticAssert = CXCursor_StaticAssert,

    { A friend declaration. }
    FriendDecl = CXCursor_FriendDecl,
    FirstExtraDecl = CXCursor_FirstExtraDecl,
    LastExtraDecl = CXCursor_LastExtraDecl,

    { A code completion overload candidate. }
    OverloadCandidate = CXCursor_OverloadCandidate);

type
  { Extends TCursorKind }
  TCursorKindHelper = record helper for TCursorKind
  private
    {$REGION 'Internal Declarations'}
    function GetSpelling: String; inline;
    {$ENDREGION 'Internal Declarations'}
  public

    { Text version of the cursor kind. }
    property Spelling: String read GetSpelling;
  end;

type
  { Describes the kind of a type }
  TTypeKind = (
    { Represents an invalid type (e.g., where no type is available). }
    Invalid = CXType_Invalid,

    { A type whose specific kind is not exposed via this interface. }
    Unexposed = CXType_Unexposed,

    { Builtin types }
    Void = CXType_Void,
    Bool = CXType_Bool,
    Char_U = CXType_Char_U,
    UChar = CXType_UChar,
    Char16 = CXType_Char16,
    Char32 = CXType_Char32,
    UShort = CXType_UShort,
    UInt = CXType_UInt,
    ULong = CXType_ULong,
    ULongLong = CXType_ULongLong,
    UInt128 = CXType_UInt128,
    Char_S = CXType_Char_S,
    SChar = CXType_SChar,
    WChar = CXType_WChar,
    Short = CXType_Short,
    Int = CXType_Int,
    Long = CXType_Long,
    LongLong = CXType_LongLong,
    Int128 = CXType_Int128,
    Float = CXType_Float,
    Double = CXType_Double,
    LongDouble = CXType_LongDouble,
    NullPtr = CXType_NullPtr,
    Overload = CXType_Overload,
    Dependent = CXType_Dependent,
    ObjCId = CXType_ObjCId,
    ObjCClass = CXType_ObjCClass,
    ObjCSel = CXType_ObjCSel,
    Float128 = CXType_Float128,
    Half = CXType_Half,
    Float16 = CXType_Float16,
    ShortAccum = CXType_ShortAccum,
    Accum = CXType_Accum,
    LongAccum = CXType_LongAccum,
    UShortAccum = CXType_UShortAccum,
    UAccum = CXType_UAccum,
    ULongAccum = CXType_ULongAccum,
    FirstBuiltin = CXType_FirstBuiltin,
    LastBuiltin = CXType_LastBuiltin,

    Complex = CXType_Complex,
    Pointer = CXType_Pointer,
    BlockPointer = CXType_BlockPointer,
    LValueReference = CXType_LValueReference,
    RValueReference = CXType_RValueReference,
    Rec = CXType_Record,
    Enum = CXType_Enum,
    Typedef = CXType_Typedef,
    ObjCInterface = CXType_ObjCInterface,
    ObjCObjectPointer = CXType_ObjCObjectPointer,
    FunctionNoProto = CXType_FunctionNoProto,
    FunctionProto = CXType_FunctionProto,
    ConstantArray = CXType_ConstantArray,
    Vector = CXType_Vector,
    IncompleteArray = CXType_IncompleteArray,
    VariableArray = CXType_VariableArray,
    DependentSizedArray = CXType_DependentSizedArray,
    MemberPointer = CXType_MemberPointer,
    Auto = CXType_Auto,

    { Represents a type that was referred to using an elaborated type keyword.
      E.g., struct S, or via a qualified name, e.g., N::M::type, or both. }
    Elaborated = CXType_Elaborated,

    { OpenCL PipeType. }
    Pipe = CXType_Pipe,

    { OpenCL builtin types. }
    OCLImage1dRO = CXType_OCLImage1dRO,
    OCLImage1dArrayRO = CXType_OCLImage1dArrayRO,
    OCLImage1dBufferRO = CXType_OCLImage1dBufferRO,
    OCLImage2dRO = CXType_OCLImage2dRO,
    OCLImage2dArrayRO = CXType_OCLImage2dArrayRO,
    OCLImage2dDepthRO = CXType_OCLImage2dDepthRO,
    OCLImage2dArrayDepthRO = CXType_OCLImage2dArrayDepthRO,
    OCLImage2dMSAARO = CXType_OCLImage2dMSAARO,
    OCLImage2dArrayMSAARO = CXType_OCLImage2dArrayMSAARO,
    OCLImage2dMSAADepthRO = CXType_OCLImage2dMSAADepthRO,
    OCLImage2dArrayMSAADepthRO = CXType_OCLImage2dArrayMSAADepthRO,
    OCLImage3dRO = CXType_OCLImage3dRO,
    OCLImage1dWO = CXType_OCLImage1dWO,
    OCLImage1dArrayWO = CXType_OCLImage1dArrayWO,
    OCLImage1dBufferWO = CXType_OCLImage1dBufferWO,
    OCLImage2dWO = CXType_OCLImage2dWO,
    OCLImage2dArrayWO = CXType_OCLImage2dArrayWO,
    OCLImage2dDepthWO = CXType_OCLImage2dDepthWO,
    OCLImage2dArrayDepthWO = CXType_OCLImage2dArrayDepthWO,
    OCLImage2dMSAAWO = CXType_OCLImage2dMSAAWO,
    OCLImage2dArrayMSAAWO = CXType_OCLImage2dArrayMSAAWO,
    OCLImage2dMSAADepthWO = CXType_OCLImage2dMSAADepthWO,
    OCLImage2dArrayMSAADepthWO = CXType_OCLImage2dArrayMSAADepthWO,
    OCLImage3dWO = CXType_OCLImage3dWO,
    OCLImage1dRW = CXType_OCLImage1dRW,
    OCLImage1dArrayRW = CXType_OCLImage1dArrayRW,
    OCLImage1dBufferRW = CXType_OCLImage1dBufferRW,
    OCLImage2dRW = CXType_OCLImage2dRW,
    OCLImage2dArrayRW = CXType_OCLImage2dArrayRW,
    OCLImage2dDepthRW = CXType_OCLImage2dDepthRW,
    OCLImage2dArrayDepthRW = CXType_OCLImage2dArrayDepthRW,
    OCLImage2dMSAARW = CXType_OCLImage2dMSAARW,
    OCLImage2dArrayMSAARW = CXType_OCLImage2dArrayMSAARW,
    OCLImage2dMSAADepthRW = CXType_OCLImage2dMSAADepthRW,
    OCLImage2dArrayMSAADepthRW = CXType_OCLImage2dArrayMSAADepthRW,
    OCLImage3dRW = CXType_OCLImage3dRW,
    OCLSampler = CXType_OCLSampler,
    OCLEvent = CXType_OCLEvent,
    OCLQueue = CXType_OCLQueue,
    OCLReserveID = CXType_OCLReserveID,
    ObjCObject = CXType_ObjCObject,
    ObjCTypeParam = CXType_ObjCTypeParam,
    Attributed = CXType_Attributed,
    OCLIntelSubgroupAVCMcePayload = CXType_OCLIntelSubgroupAVCMcePayload,
    OCLIntelSubgroupAVCImePayload = CXType_OCLIntelSubgroupAVCImePayload,
    OCLIntelSubgroupAVCRefPayload = CXType_OCLIntelSubgroupAVCRefPayload,
    OCLIntelSubgroupAVCSicPayload = CXType_OCLIntelSubgroupAVCSicPayload,
    OCLIntelSubgroupAVCMceResult = CXType_OCLIntelSubgroupAVCMceResult,
    OCLIntelSubgroupAVCImeResult = CXType_OCLIntelSubgroupAVCImeResult,
    OCLIntelSubgroupAVCRefResult = CXType_OCLIntelSubgroupAVCRefResult,
    OCLIntelSubgroupAVCSicResult = CXType_OCLIntelSubgroupAVCSicResult,
    OCLIntelSubgroupAVCImeResultSingleRefStreamout = CXType_OCLIntelSubgroupAVCImeResultSingleRefStreamout,
    OCLIntelSubgroupAVCImeResultDualRefStreamout = CXType_OCLIntelSubgroupAVCImeResultDualRefStreamout,
    OCLIntelSubgroupAVCImeSingleRefStreamin = CXType_OCLIntelSubgroupAVCImeSingleRefStreamin,
    OCLIntelSubgroupAVCImeDualRefStreamin = CXType_OCLIntelSubgroupAVCImeDualRefStreamin,
    ExtVector = CXType_ExtVector);

type
  { Describes the kind of a template argument. }
  TTemplateArgumentKind = (
    Null = CXTemplateArgumentKind_Null,
    Typ = CXTemplateArgumentKind_Type,
    Declaration = CXTemplateArgumentKind_Declaration,
    NullPtr = CXTemplateArgumentKind_NullPtr,
    Integral = CXTemplateArgumentKind_Integral,
    Template = CXTemplateArgumentKind_Template,
    TemplateExpansion = CXTemplateArgumentKind_TemplateExpansion,
    Expression = CXTemplateArgumentKind_Expression,
    Pack = CXTemplateArgumentKind_Pack,

    { Indicates an error case, preventing the kind from being deduced. }
    Invalid = CXTemplateArgumentKind_Invalid);

type
  { The linkage of the entity referred to by a cursor. }
  TLinkageKind = (
    { This value indicates that no linkage information is available for a
      provided TCursor. }
    Invalid = CXLinkage_Invalid,

    { This is the linkage for variables, parameters, and so on that have
      automatic storage.  This covers normal (non-extern) local variables. }
    NoLinkage = CXLinkage_NoLinkage,

    { This is the linkage for static variables and static functions. }
    Internal = CXLinkage_Internal,

    { This is the linkage for entities with external linkage that live in C++
      anonymous namespaces. }
    UniqueExternal = CXLinkage_UniqueExternal,

    { This is the linkage for entities with true, external linkage. }
    External = CXLinkage_External);

type
  { The visibility of an entity referenced by a cursor. }
  TVisibility = (
    { This value indicates that no visibility information is available for a
      provided TCursor. }
    Invalid = CXVisibility_Invalid,

    { Symbol not seen by the linker. }
    Hidden = CXVisibility_Hidden,

    { Symbol seen by the linker but resolves to a symbol inside this object. }
    Protected = CXVisibility_Protected,

    { Symbol seen by the linker and acts like a normal symbol. }
    Default = CXVisibility_Default);

type
  { The availability of a particular entity, which indicates whether the use of
    this entity will result in a warning or error due to it being deprecated or
    unavailable. }
  TAvailabilityKind = (
    { The entity is available. }
    Available = CXAvailability_Available,

    { The entity is available, but has been deprecated (and its use is not
      recommended). }
    Deprecated = CXAvailability_Deprecated,

    { The entity is not available; any use of it will be an error. }
    NotAvailable = CXAvailability_NotAvailable,

    { The entity is available, but not accessible; any use of it will be an
      error. }
    NotAccessible = CXAvailability_NotAccessible);

type
  { The "language" of the entity referred to by a cursor }
  TLanguageKind = (
    Invalid = CXLanguage_Invalid,
    C = CXLanguage_C,
    ObjC = CXLanguage_ObjC,
    CPlusPlus = CXLanguage_CPlusPlus);

type
  { The C++ access control level to a base class for a cursor with kind
    CXXBaseSpecifier. }
  TCxxAccessSpecifier = (
    Invalid = CX_CXXInvalidAccessSpecifier,
    Public = CX_CXXPublic,
    Protected = CX_CXXProtected,
    Private = CX_CXXPrivate);

type
  { The storage classes as declared in the source. Option Invalid was added for
    the case that the passed cursor in not a declaration. }
  TStorageClass = (
    Invalid = CX_SC_Invalid,
    None = CX_SC_None,
    Extern = CX_SC_Extern,
    Static = CX_SC_Static,
    PrivateExtern = CX_SC_PrivateExtern,
    OpenCLWorkGroupLocal = CX_SC_OpenCLWorkGroupLocal,
    Auto = CX_SC_Auto,
    Register = CX_SC_Register);

type
  { The "thread-local storage (TLS) kind" of the declaration referred to by a
    cursor. }
  TTlsKind = (
    None = CXTLS_None,
    Dynamic = CXTLS_Dynamic,
    Static = CXTLS_Static);

type
  { Property attributes for a ObjCPropertyDecl cursors. }
  TObjCPropertyAttrKind = (
    ReadOnly,
    Getter,
    Assign,
    ReadWrite,
    Retain,
    Copy,
    NonAtomic,
    Setter,
    Atomic,
    Weak,
    Strong,
    UnsafeRetained,
    Clas);
  TObjCPropertyAttrKinds = set of TObjCPropertyAttrKind;

type
  { 'Qualifiers' written next to the return and parameter types in Objective-C
    method declarations. }
  TObjCDeclQualifierKind = (
    Input,
    InOut,
    Output,
    ByCopy,
    ByRef,
    OneWay);
  TObjCDeclQualifierKinds = set of TObjCDeclQualifierKind;

type
  { Name reference flags }
  TNameRefFlag = (
    { Include the nested-name-specifier, e.g. Foo:: in x.Foo::y, in the range.}
    WantQualifier,

    { Include the explicit template arguments, e.g. <int> in x.f<int>, in the
      range. }
    WantTemplateArgs,

    { If the name is non-contiguous, return the full spanning range.
      Non-contiguous names occur in Objective-C when a selector with two or more
      parameters is used, or in C++ when using an operator:

        [object doSomething:here withValue:there]; // Objective-C
        return some_vector[1]; // C++ }
    WantSinglePiece);
  TNameRefFlags = set of TNameRefFlag;

type
  { Describes the kind of error that occurred (if any) in a call to
    TDiagnosticSet.Load }
  TLoadDiagError = (
    { Indicates that no error occurred. }
    None = CXLoadDiag_None,

    { Indicates that an unknown error occurred while attempting to deserialize
      diagnostics. }
    Unknown = CXLoadDiag_Unknown,

    { Indicates that the file containing the serialized diagnostics could not
      be opened. }
    CannotLoad = CXLoadDiag_CannotLoad,

    { Indicates that the serialized diagnostics file is invalid or corrupt. }
    InvalidFile = CXLoadDiag_InvalidFile);

type
  { Options to control the display of diagnostics. }
  TDiagnosticDisplayOption = (
    { Display the source-location information where the diagnostic was located.

      When set, diagnostics will be prefixed by the file, line, and (optionally)
      column to which the diagnostic refers. For example,

      @preformatted(
        test.c:28: warning: extra tokens at end of #endif directive
      )

      This option corresponds to the clang flag -fshow-source-location. }
    DisplaySourceLocation,

    { If displaying the source-location information of the diagnostic, also
      include the column number.

      This option corresponds to the clang flag -fshow-column. }
    DisplayColumn,

    { If displaying the source-location information of the diagnostic, also
      include information about source ranges in a machine-parsable format.

      This option corresponds to the clang flag
      -fdiagnostics-print-source-range-info. }
    DisplaySourceRanges,

    { Display the option name associated with this diagnostic, if any.

      The option name displayed (e.g., -Wconversion) will be placed in brackets
      after the diagnostic text. This option corresponds to the clang flag
      -fdiagnostics-show-option. }
    DisplayOption,

    { Display the category number associated with this diagnostic, if any.

      The category number is displayed within brackets after the diagnostic
      text. This option corresponds to the clang flag
      -fdiagnostics-show-category=id. }
    DisplayCategoryId,

    { Display the category name associated with this diagnostic, if any.

      The category name is displayed within brackets after the diagnostic text.
      This option corresponds to the clang flag
      -fdiagnostics-show-category=name. }
    DisplayCategoryName);
  TDiagnosticDisplayOptions = set of TDiagnosticDisplayOption;

type
  { The severity of a particular diagnostic. }
  TDiagnosticSeverity = (
    { A diagnostic that has been suppressed, e.g., by a command-line option. }
    Ignored = CXDiagnostic_Ignored,

    { This diagnostic is a note that should be attached to the previous
      (non-note) diagnostic. }
    Note = CXDiagnostic_Note,

    { This diagnostic indicates suspicious code that may not be wrong. }
    Warning = CXDiagnostic_Warning,

    { This diagnostic indicates that the code is ill-formed. }
    Error = CXDiagnostic_Error,

    { This diagnostic indicates that the code is ill-formed such that future
      parser recovery is unlikely to produce useful results. }
    Fatal = CXDiagnostic_Fatal);

type
  { Categorizes how memory is being used by a translation unit. }
  TResourceUsageKind = (
    AST = CXTUResourceUsage_AST,
    Identifiers = CXTUResourceUsage_Identifiers,
    Selectors = CXTUResourceUsage_Selectors,
    GlobalCompletionResults = CXTUResourceUsage_GlobalCompletionResults,
    SourceManagerContentCache = CXTUResourceUsage_SourceManagerContentCache,
    AST_SideTables = CXTUResourceUsage_AST_SideTables,
    SourceManager_Membuffer_Malloc = CXTUResourceUsage_SourceManager_Membuffer_Malloc,
    SourceManager_Membuffer_MMap = CXTUResourceUsage_SourceManager_Membuffer_MMap,
    ExternalASTSource_Membuffer_Malloc = CXTUResourceUsage_ExternalASTSource_Membuffer_Malloc,
    ExternalASTSource_Membuffer_MMap = CXTUResourceUsage_ExternalASTSource_Membuffer_MMap,
    Preprocessor = CXTUResourceUsage_Preprocessor,
    PreprocessingRecord = CXTUResourceUsage_PreprocessingRecord,
    SourceManager_DataStructures = CXTUResourceUsage_SourceManager_DataStructures,
    Preprocessor_HeaderSearch = CXTUResourceUsage_Preprocessor_HeaderSearch);

type
  { Describes the calling convention of a function type }
  TCallingConv = (
    Default = CXCallingConv_Default,
    C = CXCallingConv_C,
    X86StdCall = CXCallingConv_X86StdCall,
    X86FastCall = CXCallingConv_X86FastCall,
    X86ThisCall = CXCallingConv_X86ThisCall,
    X86Pascal = CXCallingConv_X86Pascal,
    AAPCS = CXCallingConv_AAPCS,
    AAPCS_VFP = CXCallingConv_AAPCS_VFP,
    X86RegCall = CXCallingConv_X86RegCall,
    IntelOclBicc = CXCallingConv_IntelOclBicc,
    Win64 = CXCallingConv_Win64,
    X86_64Win64 = CXCallingConv_X86_64Win64,
    X86_64SysV = CXCallingConv_X86_64SysV,
    X86VectorCall = CXCallingConv_X86VectorCall,
    Swift = CXCallingConv_Swift,
    PreserveMost = CXCallingConv_PreserveMost,
    PreserveAll = CXCallingConv_PreserveAll,
    AArch64VectorCall = CXCallingConv_AArch64VectorCall,
    Invalid = CXCallingConv_Invalid,
    Unexposed = CXCallingConv_Unexposed);

type
  { Kinds of reference qualifiers }
  TRefQualifierKind = (
    { No ref-qualifier was provided. }
    None = CXRefQualifier_None,

    { An lvalue ref-qualifier was provided (&). }
    LValue = CXRefQualifier_LValue,

    { An rvalue ref-qualifier was provided (&&). }
    RValue = CXRefQualifier_RValue);

type
  { A kind of token. }
  TTokenKind = (
    { A token that contains some kind of punctuation. }
    Punctuation = CXToken_Punctuation,

    { A language keyword. }
    Keyword = CXToken_Keyword,

    { An identifier (that is not a keyword). }
    Identifier = CXToken_Identifier,

    { A numeric, string, or character literal. }
    Literal = CXToken_Literal,

    { A comment. }
    Comment = CXToken_Comment);

type
  { Describes a single piece of text within a code-completion string.

    Each "chunk" within a code-completion string TCompletionString) is either a
    piece of text with a specific "kind" that describes how that text should be
    interpreted by the client or is another completion string. }
  TCompletionChunkKind = (
    { A code-completion string that describes "optional" text that could be a
      part of the template (but is not required).

      The Optional chunk is the only kind of chunk that has a code-completion
      string for its representation, which is accessible via
      TCompletionString.ChunkCompletionString. The code-completion string
      describes an additional part of the template that is completely optional.
      For example, optional chunks can be used to describe the placeholders for
      arguments that match up with defaulted function parameters, e.g. given:

      @preformatted(
        void f(int x, float y = 3.14, double z = 2.71828);
      )

      The code-completion string for this function would contain:
      @preformatted(
      - a TypedText chunk for "f".
      - a LeftParen chunk for "(".
      - a Placeholder chunk for "int x"
      - an Optional chunk containing the remaining defaulted arguments, e.g.,
        - a Comma chunk for ","
        - a Placeholder chunk for "float y"
        - an Optional chunk containing the last defaulted argument:
        - a Comma chunk for ","
        - a Placeholder chunk for "double z"
      - a RightParen chunk for ")"
      )

      There are many ways to handle Optional chunks. Two simple approaches are:
      @preformatted(
      - Completely ignore optional chunks, in which case the template for the
        function "f" would only include the first parameter ("int x").
      - Fully expand all optional chunks, in which case the template for the
        function "f" would have all of the parameters.
      ) }
  Optional = CXCompletionChunk_Optional,

  { Text that a user would be expected to type to get this code-completion
    result.

    There will be exactly one "typed text" chunk in a semantic string, which
    will typically provide the spelling of a keyword or the name of a
    declaration that could be used at the current code point. Clients are
    expected to filter the code-completion results based on the text in this
    chunk. }
  TypedText = CXCompletionChunk_TypedText,

  { Text that should be inserted as part of a code-completion result.

    A "text" chunk represents text that is part of the template to be
    inserted into user code should this particular code-completion result
    be selected. }
  Text = CXCompletionChunk_Text,

  { Placeholder text that should be replaced by the user.

    A "placeholder" chunk marks a place where the user should insert text
    into the code-completion template. For example, placeholders might mark
    the function parameters for a function declaration, to indicate that the
    user should provide arguments for each of those parameters. The actual
    text in a placeholder is a suggestion for the text to display before
    the user replaces the placeholder with real code. }
  Placeholder = CXCompletionChunk_Placeholder,

  { Informative text that should be displayed but never inserted as part of the
    template.

    An "informative" chunk contains annotations that can be displayed to
    help the user decide whether a particular code-completion result is the
    right option, but which is not part of the actual template to be inserted
    by code completion. }
  Informative = CXCompletionChunk_Informative,

  { Text that describes the current parameter when code-completion is referring
    to function call, message send, or template specialization.

    A "current parameter" chunk occurs when code-completion is providing
    information about a parameter corresponding to the argument at the
    code-completion point. For example, given a function

    @preformatted(
      int add(int x, int y);
    )

    and the source code add(, where the code-completion point is after the
    "(", the code-completion string will contain a "current parameter" chunk
    for "int x", indicating that the current argument will initialize that
    parameter. After typing further, to add(17, (where the code-completion
    point is after the ","), the code-completion string will contain a
    "current paremeter" chunk to "int y". }
  CurrentParameter = CXCompletionChunk_CurrentParameter,

  { A left parenthesis ('('), used to initiate a function call or signal the
    beginning of a function parameter list. }
  LeftParen = CXCompletionChunk_LeftParen,

  { A right parenthesis (')'), used to finish a function call or signal the end
    of a function parameter list. }
  RightParent = CXCompletionChunk_RightParen,

  { A left bracket ('['). }
  LeftBracket = CXCompletionChunk_LeftBracket,

  { A right bracket (']'). }
  RightBracket = CXCompletionChunk_RightBracket,

  (*A left brace ('{'). *)
  LeftBrace = CXCompletionChunk_LeftBrace,

  (*A right brace ('}'). *)
  RightBrace = CXCompletionChunk_RightBrace,

  { A left angle bracket ('<'). }
  LeftAngle = CXCompletionChunk_LeftAngle,

  { A right angle bracket ('>'). }
  RightAngle = CXCompletionChunk_RightAngle,

  { A comma separator (','). }
  Comma = CXCompletionChunk_Comma,

  { Text that specifies the result type of a given result.

    This special kind of informative chunk is not meant to be inserted into
    the text buffer. Rather, it is meant to illustrate the type that an
    expression using the given completion string would have. }
  ResultType = CXCompletionChunk_ResultType,

  { A colon (':'). }
  Colon = CXCompletionChunk_Colon,

  { A semicolon (';'). }
  SemiColon = CXCompletionChunk_SemiColon,

  { An '=' sign. }
  Equal = CXCompletionChunk_Equal,

  { Horizontal space (' '). }
  HorizontalSpace = CXCompletionChunk_HorizontalSpace,

  { Vertical space (#10), after which it is generally a good idea to perform
    indentation. }
  VerticalSpace = CXCompletionChunk_VerticalSpace);

type
  { Kinds of evaluation results }
  TEvalResultKind = (
    Unexposed = CXEval_UnExposed,
    Int = CXEval_Int,
    Float = CXEval_Float,
    ObjCStrLiteral = CXEval_ObjCStrLiteral,
    StrLiteral = CXEval_StrLiteral,
    CFStr = CXEval_CFStr,
    Other = CXEval_Other);

type
  { Indexer options used by IIndexAction }
  TIndexOption = (
    { Used to indicate that IIndexerListener.IndexEntityReference should be
      invoked for only one reference of an entity per source file that does
      not also include a declaration/definition of the entity. }
    SuppressRedundantRefs,

    { Function-local symbols should be indexed. If this is not set
      function-local symbols will be ignored. }
    IndexFunctionLocalSymbols,

    { Implicit function/class template instantiations should be indexed. If
      this is not set, implicit instantiations will be ignored. }
    IndexImplicitTemplateInstantiations,

    { Suppress all compiler warnings when parsing for indexing. }
    SuppressWarnings,

    { Skip a function/method body that was already parsed during an indexing
      session associated with an IIndexAction object. Bodies in system headers
      are always skipped. }
    SkipParsedBodiesInSession);
  TIndexOptions = set of TIndexOption;

type
  { Describes the type of the comment AST node (TComment). A comment node can be
    considered block content (e. g., paragraph), inline content (plain text) or
    neither (the root AST node). }
  TCommentKind = (
    { Null comment. No AST node is constructed at the requested location because
      there is no text or a syntax error. }
    Null = CXComment_Null,

    { Plain text. Inline content. }
    Text = CXComment_Text,

    { A command with word-like arguments that is considered inline content.

      For example: \c command. }
    InlineCommand = CXComment_InlineCommand,

    { HTML start tag with attributes (name-value pairs). Considered inline
      content. For example:

      @preformatted(
        <br> <br /> <a href="http://example.org/">
      ) }
    HtmlStartTag = CXComment_HTMLStartTag,

    { HTML end tag. Considered inline content. For example:

      @preformatted(
        </a>
      ) }
    HtmlEndTag = CXComment_HTMLEndTag,

    { A paragraph, contains inline comment. The paragraph itself is block
      content. }
    Paragraph = CXComment_Paragraph,

    { A command that has zero or more word-like arguments (number of word-like
      arguments depends on command name) and a paragraph as an argument. Block
      command is block content.

      Paragraph argument is also a child of the block command.

      For example: \brief has 0 word-like arguments and a paragraph argument.

      AST nodes of special kinds that parser knows about (e.g., \param command)
      have their own node kinds. }
    BlockCommand = CXComment_BlockCommand,

    { A \param or \arg command that describes the function parameter (name,
      passing direction, description).

      For example: \param [in] ParamName description. }
    ParamCommand = CXComment_ParamCommand,

    { A \tparam command that describes a template parameter (name and
      description).

      For example: \tparam T description. }
    TParamCommand = CXComment_TParamCommand,

    { A verbatim block command (e. g., preformatted code). Verbatim block has an
      opening and a closing command and contains multiple lines of text
      (VerbatimBlockLine child nodes).


      For example:

      @preformatted(
        \verbatim
        aaa
        \endverbatim
      ) }
    VerbatimBlockCommand = CXComment_VerbatimBlockCommand,

    { A line of text that is contained within a VerbatimBlockCommand node. }
    VerbatimBlockLine = CXComment_VerbatimBlockLine,

    { A verbatim line command. Verbatim line has an opening command, a single
      line of text (up to the newline after the opening command) and has no
      closing command. }
    VerbatimLine = CXComment_VerbatimLine,

    { A full comment attached to a declaration, contains block content. }
    FullComment = CXComment_FullComment);

type
  { The most appropriate rendering mode for an inline command, chosen on command
    semantics in Doxygen. }
  TCommentInlineCommandRenderKind = (
    { Command argument should be rendered in a normal font. }
    Normal = CXCommentInlineCommandRenderKind_Normal,

    { Command argument should be rendered in a bold font. }
    Bold = CXCommentInlineCommandRenderKind_Bold,

    { Command argument should be rendered in a monospaced font. }
    Monospaced = CXCommentInlineCommandRenderKind_Monospaced,

    { Command argument should be rendered emphasized (typically italic font). }
    Emphasized = CXCommentInlineCommandRenderKind_Emphasized);

type
  { Describes parameter passing direction for \param or \arg command. }
  TCommentParamPassDirection = (
    { The parameter is an input parameter. }
    Input = CXCommentParamPassDirection_In,

    { The parameter is an output parameter. }
    Ouput = CXCommentParamPassDirection_Out,

    { The parameter is an input and output parameter. }
    InOut = CXCommentParamPassDirection_InOut);

const
  { List the possible error codes for TType.SizeOf, TType.AlignOf,
    TType.OffsetOf and TCursor.OffsetOfField. }

  { Type is of kind TTypeKind.Invalid. }
  TYPE_LAYOUT_ERROR_INVALID =  CXTypeLayoutError_Invalid;

  { The type is an incomplete Type }
  TYPE_LAYOUT_ERROR_INCOMPLETE = CXTypeLayoutError_Incomplete;

  { The type is a dependent Type. }
  TYPE_LAYOUT_ERROR_DEPENDENT = CXTypeLayoutError_Dependent;

  { The type is not a constant size type. }
  TYPE_LAYOUT_ERROR_NOT_CONSTANT_SIZE = CXTypeLayoutError_NotConstantSize;

  { The Field name is not valid for this record. }
  TYPE_LAYOUT_ERROR_INVALID_FIELDNAME = CXTypeLayoutError_InvalidFieldName;

  { The type is undeduced. }
  TYPE_LAYOUT_ERROR_UNDEDUCED = CXTypeLayoutError_Undeduced;

type
  { Describes a version number of the form major.minor.subminor. }
  TVersion = record
  public
    { The major version number, e.g., the '10' in '10.7.3'. A negative value
      indicates that there is no version number at all. }
    Major: Integer;

    { The minor version number, e.g., the '7' in '10.7.3'. This value will be
      negative if no minor version number was provided, e.g., for version '10'. }
    Minor: Integer;

    { The subminor version number, e.g., the '3' in '10.7.3'. This value will be
      negative if no minor or subminor version number was provided, e.g., in
      version '10' or '10.7'. }
    Subminor: Integer;
  end;

type
  { Describes the availability of a given entity on particular platforms, e.g.,
    a particular class might only be available on Mac OS 10.7 or newer. }
  TPlatformAvailability = record
  public
    { Whether the entity is deprecated on all platforms. }
    AlwaysDeprecated: Boolean;

    { The message text provided along with the unconditional deprecation of this
      entity. }
    DeprecatedMessage: String;

    { Whether the entity is unavailable on all platforms. }
    AlwaysUnavailable: Boolean;

    { The message text provided along with the unconditional unavailability of
      this entity. }
    UnavailableMessage: String;

    { Platform-specific availability }
    Platforms: array of record
      { A string that describes the platform for which this structure provides
        availability information.
        Possible values are "ios" or "macos". }
      Name: String;

      { The version number in which this entity was introduced. }
      Introduced: TVersion;

      { The version number in which this entity was deprecated (but is still
        available). }
      DeprecatedVersion: TVersion;

      { The version number in which this entity was obsoleted, and therefore
        is no longer available. }
      ObsoletedVersion: TVersion;

      { Whether the entity is unconditionally unavailable on this platform. }
      Unavailable: Boolean;

      { An optional message to provide to a user of this API, e.g., to suggest
        replacement APIs. }
      Message: String;
    end;
  end;

type
  { Describes the exception specification of a cursor. }
  TExceptionSpecificationKind  = (
    { Indicates that the cursor is not a function declaration. }
    Error = -1,

    { The cursor has no exception specification. }
    None = CXCursor_ExceptionSpecificationKind_None,

    { The cursor has exception specification throw() }
    DynamicNone = CXCursor_ExceptionSpecificationKind_DynamicNone,

    { The cursor has exception specification throw(T1, T2) }
    Dynamic = CXCursor_ExceptionSpecificationKind_Dynamic,

    { The cursor has exception specification throw(...). }
    MSAny = CXCursor_ExceptionSpecificationKind_MSAny,

    { The cursor has exception specification basic noexcept. }
    BasicNoexcept = CXCursor_ExceptionSpecificationKind_BasicNoexcept,

    { The cursor has exception specification computed noexcept. }
    ComputedNoexcept = CXCursor_ExceptionSpecificationKind_ComputedNoexcept,

    { The exception specification has not yet been evaluated. }
    Unevalutated = CXCursor_ExceptionSpecificationKind_Unevaluated,

    { The exception specification has not yet been instantiated. }
    Uninstantiated = CXCursor_ExceptionSpecificationKind_Uninstantiated,

    { The exception specification has not been parsed yet. }
    Unparsed = CXCursor_ExceptionSpecificationKind_Unparsed,

    { The cursor has a __declspec(nothrow) exception specification. }
    NoThrow = CXCursor_ExceptionSpecificationKind_NoThrow);

type
  TTypeNullabilityKind = (
    { Values of this type can never be null. }
    NonNull = CXTypeNullability_NonNull,

    { Values of this type can be null. }
    Nullable = CXTypeNullability_Nullable,

    { Whether values of this type can be null is (explicitly)
      unspecified. This captures a (fairly rare) case where we
      can't conclude anything about the nullability of the type even
      though it has been considered. }
    Unspecified = CXTypeNullability_Unspecified,

    { Nullability is not applicable to this type. }
    Invalid = CXTypeNullability_Invalid);

type
  { The type of an element in the abstract syntax tree. }
  TType = record
  {$REGION 'Internal Declarations'}
  private
    FHandle: TCXType;
    function GetKind: TTypeKind; inline;
    function GetKindSpelling: String;
    function GetSpelling: String; inline;
    function GetCanonicalType: TType; inline;
    function GetIsConstQualified: Boolean; inline;
    function GetIsVolatileQualified: Boolean; inline;
    function GetIsRestrictQualified: Boolean; inline;
    function GetIsFunctionVariadic: Boolean; inline;
    function GetIsPodType: Boolean; inline;
    function GetIsTransparentTagTypedef: Boolean; inline;
    function GetAddressSpace: Cardinal; inline;
    function GetTypedefName: String; inline;
    function GetPointeeType: TType; inline;
    function GetObjCEncoding: String; inline;
    function GetFunctionCallingConv: TCallingConv; inline;
    function GetResultType: TType; inline;
    function GetExceptionSpecificationType: TExceptionSpecificationKind; inline;
    function GetArgTypeCount: Integer; inline;
    function GetArgType(const AIndex: Integer): TType; inline;
    function GetElementType: TType; inline;
    function GetElementCount: Integer; inline;
    function GetArrayElementType: TType; inline;
    function GetArraySize: Int64; inline;
    function GetNamedType: TType; inline;
    function GetAlignOf: Int64; inline;
    function GetClassType: TType; inline;
    function GetSizeOf: Int64; inline;
    function GetTemplateArgumentCount: Integer; inline;
    function GetTemplateArgumentType(const AIndex: Integer): TType; inline;
    function GetCxxRefQualifier: TRefQualifierKind; inline;
    function GetObjCObjectBaseType: TType; inline;
    function GetObjCProtocolDecl(const AIndex: Integer): TCXCursor; inline;
    function GetObjCProtocolRefCount: Integer; inline;
    function GetObjCTypeArg(const AIndex: Integer): TType; inline;
    function GetObjCTypeArgCount: Integer; inline;
    function GetNullability: TTypeNullabilityKind; inline;
    function GetModifiedType: TType; inline;
  {$ENDREGION 'Internal Declarations'}
  public
    { Equality operators. Determine whether two TType's represent the same
      type. }
    class operator Equal(const ALeft, ARight: TType): Boolean; inline; static;
    class operator NotEqual(const ALeft, ARight: TType): Boolean; inline; static;

    { Get the offset of a field named AFieldName in a record of this type in
      bits as it would be returned by __offsetof__ as per C++11[18.2p4]

      * If the cursor is not a record field declaration,
        TYPE_LAYOUT_ERROR_INVALID is returned.
      * If the field's type declaration is an incomplete type,
        TYPE_LAYOUT_ERROR_INCOMPLETE is returned.
      * If the field's type declaration is a dependent type,
        TYPE_LAYOUT_ERROR_DEPENDENT is returned.
      * If the field's name S is not found, TYPE_LAYOUT_ERROR_INVALID_FIELDNAME
        is returned. }
    function GetOffsetOf(const AFieldName: String): Int64; inline;

    { The kind of the type }
    property Kind: TTypeKind read GetKind;

    { The spelling of the type kind }
    property KindSpelling: String read GetKindSpelling;

    { Pretty-print the underlying type using the rules of the language of the
      translation unit from which it came.

      If the type is invalid, an empty string is returned. }
    property Spelling: String read GetSpelling;

    { The canonical type for the type.

      Clang's type system explicitly models typedefs and all the ways a specific
      type can be represented. The canonical type is the underlying type with
      all the "sugar" removed. For example, if 'T' is a typedef for 'int', the
      canonical type for 'T' would be 'int'. }
    property CanonicalType: TType read GetCanonicalType;

    { Whether this type has the "const" qualifier set, without looking through
      typedefs that may have added "const" at a different level. }
    property IsConstQualified: Boolean read GetIsConstQualified;

    { Whether this type has the "volatile" qualifier set, without looking
      through typedefs that may have added "volatile" at a different level. }
    property IsVolatileQualified: Boolean read GetIsVolatileQualified;

    { Whether this type has the "restrict" qualifier set, without looking
      through typedefs that may have added "restrict" at a different level. }
    property IsRestrictQualified: Boolean read GetIsRestrictQualified;

    { Whether the type is a variadic function type. }
    property IsFunctionVariadic: Boolean read GetIsFunctionVariadic;

    { Whether the type is a POD (plain old data) type. }
    property IsPodType: Boolean read GetIsPodType;

    { Whether a typedef is 'transparent' tag.

      A typedef is considered 'transparent' if it shares a name and spelling
      location with its underlying tag type, as is the case with the NS_ENUM
      macro. }
    property IsTransparentTagTypedef: Boolean read GetIsTransparentTagTypedef;

    { The address space of the type. }
    property AddressSpace: Cardinal read GetAddressSpace;

    { The typedef name of the type. }
    property TypedefName: String read GetTypedefName;

    { For pointer types, the type of the pointee. }
    property PointeeType: TType read GetPointeeType;

    { The Objective-C type encoding for the type. }
    property ObjCEncoding: String read GetObjCEncoding;

    { The calling convention associated with a function type.

      If a non-function type is passed in, nvalid is returned.}
    property FunctionCallingConv: TCallingConv read GetFunctionCallingConv;

    { The return type associated with a function type.

      If a non-function type is passed in, an invalid type is returned. }
    property ResultType: TType read GetResultType;

    { The exception specification type associated with a function type.

      If a non-function type is passed in, TExceptionSpecificationKind.Error is
      returned. }
    property ExceptionSpecificationType: TExceptionSpecificationKind read GetExceptionSpecificationType;

    { The number of non-variadic parameters associated with a function type.

      If a non-function type is passed in, -1 is returned. }
    property ArgTypeCount: Integer read GetArgTypeCount;

    { The type of a parameter of a function type.

      If a non-function type is passed in or the function does not have enough
      parameters, an invalid type is returned. }
    property ArgTypes[const AIndex: Integer]: TType read GetArgType;

    { The element type of an array, complex, or vector type.

      If the type is not an array, complex, or vector type, an invalid type is
      returned. }
    property ElementType: TType read GetElementType;

    { The number of elements of an array or vector type.

      If the type is not an array or vector type, -1 is returned. }
    property ElementCount: Integer read GetElementCount;

    { The element type of an array type.

      For non-array types, an invalid type is returned.}
    property ArrayElementType: TType read GetArrayElementType;

    { the array size of a constant array.

      For a non-array types, -1 is returned }
    property ArraySize: Int64 read GetArraySize;

    { The type named by the qualified-id.

      For non-elaborated types, an invalid type is returned. }
    property NamedType: TType read GetNamedType;

    { The alignment of the type in bytes as per C++[expr.alignof] standard.

      * If the type declaration is invalid, TYPE_LAYOUT_ERROR_INVALID is
        returned.
      * If the type declaration is an incomplete type,
        TYPE_LAYOUT_ERROR_INCOMPLETE is returned.
      * If the type declaration is a dependent type, TYPE_LAYOUT_ERROR_DEPENDENT
        is returned.
      * If the type declaration is not a constant size type,
        TYPE_LAYOUT_ERROR_NOT_CONSTANT_SIZE is returned. }
    property AlignOf: Int64 read GetAlignOf;

    { The class type of an member pointer type.

      For non-member-pointer types, an invalid type is returned.}
    property ClassType: TType read GetClassType;

    { The size of a type in bytes as per C++[expr.sizeof] standard.

      * If the type declaration is invalid, TYPE_LAYOUT_ERROR_INVALID is
        returned.
      * If the type declaration is an incomplete type,
        TYPE_LAYOUT_ERROR_INCOMPLETE is returned.
      * If the type declaration is a dependent type, TYPE_LAYOUT_ERROR_DEPENDENT
        is returned. }
    property SizeOf: Int64 read GetSizeOf;

    { The number of template arguments for this template specialization, or -1
      if this type is not a template specialization. }
    property TemplateArgumentCount: Integer read GetTemplateArgumentCount;

    { The type template argument of this template class specialization at given
      index.

      This property only returns template type arguments and does not handle
      template template arguments or variadic packs.}
    property TemplateArgumentTypes[const AIndex: Integer]: TType read GetTemplateArgumentType;

    { The ref-qualifier kind of a function or method.

      The ref-qualifier is returned for C++ functions or methods. For other
      types or non-C++ declarations, None is returned. }
    property CxxRefQualifier: TRefQualifierKind read GetCxxRefQualifier;

    { The base type of the ObjCObjectType.
      If the type is not an ObjC object, an invalid type is returned. }
    property ObjCObjectBaseType: TType read GetObjCObjectBaseType;

    { The number of protocol references associated with an ObjC object/id.
      If the type is not an ObjC object, 0 is returned. }
    property ObjCProtocolRefCount: Integer read GetObjCProtocolRefCount;

    { The decls for a protocol reference for an ObjC object/id.
      If the type is not an ObjC object or there are not enough protocol
      references, an invalid cursor is returned. }
    property ObjCProtocolDecls[const AIndex: Integer]: TCXCursor read GetObjCProtocolDecl;

    { The number of type arguments associated with an ObjC object.
      If the type is not an ObjC object, 0 is returned. }
    property ObjCTypeArgCount: Integer read GetObjCTypeArgCount;

    { The type arguments associated with an ObjC object.
      If the type is not an ObjC or the index is not valid,
      an invalid type is returned. }
    property ObjCTypeArgs[const AIndex: Integer]: TType read GetObjCTypeArg;

    { The nullability kind of a pointer type. }
    property Nullability: TTypeNullabilityKind read GetNullability;

    { The type that was modified by this attributed type.
      If the type is not an attributed type, an invalid type is returned. }
    property ModifiedType: TType read GetModifiedType;

    { Internal handle to C API }
    property Handle: TCXType read FHandle;
  end;

type
  { Uniquely identifies a TFile, that refers to the same underlying file,
    across an indexing session. }
  TFileUniqueId = record
  {$REGION 'Internal Declarations'}
  private
    FHandle: TCXFileUniqueID;
  {$ENDREGION 'Internal Declarations'}
  public
    { Equality operators }
    class operator Equal(const ALeft, ARight: TFileUniqueId): Boolean; inline; static;
    class operator NotEqual(const ALeft, ARight: TFileUniqueId): Boolean; inline; static;

    { Whether the file id is valid (eg. not all zero). }
    function IsValid: Boolean;

    { Internal handle to C API }
    property Handle: TCXFileUniqueID read FHandle;
  end;

type
  { A particular source file that is part of a translation unit. }
  TFile = record
  {$REGION 'Internal Declarations'}
  private
    FHandle: TCXFile;
    function GetFilename: String; inline;
    function GetFileTime: TDateTime; inline;
    function GetUniqueId: TFileUniqueId; inline;
    function GetRealPathName: String; inline;
  {$ENDREGION 'Internal Declarations'}
  public
    { Equality operators. Check if two TFile objects point to the same file,
      or they are both Null. }
    class operator Equal(const ALeft, ARight: TFile): Boolean; inline; static;
    class operator NotEqual(const ALeft, ARight: TFile): Boolean; inline; static;

    { Whether the file handle is assigned or not. }
    function IsNull: Boolean; inline;

    { The complete file and path name of the given file. }
    property Filename: String read GetFilename;

    { The last modification time of the given file. }
    property FileTime: TDateTime read GetFileTime;

    { The unique ID of the file.
      Returns an invalid ID on error. }
    property UniqueId: TFileUniqueId read GetUniqueId;

    { The real path name.
      An empty string may be returned. Use FileName in that case. }
    property RealPathName: String read GetRealPathName;

    { Internal handle to C API }
    property Handle: TCXFile read FHandle;
  end;

type
  { Identifies a specific source location within a translation unit. }
  TSourceLocation = record
  {$REGION 'Internal Declarations'}
  private
    FHandle: TCXSourceLocation;
    function GetIsInSystemHeader: Boolean; inline;
    function GetIsFromMainFile: Boolean; inline;
  {$ENDREGION 'Internal Declarations'}
  public
    { Equality operators. Determine whether two source locations, which must
      refer into the same translation unit, refer to exactly the same point in
      the source code. }
    class operator Equal(const ALeft, ARight: TSourceLocation): Boolean; inline; static;
    class operator NotEqual(const ALeft, ARight: TSourceLocation): Boolean; inline; static;

    { A Null (invalid) source location }
    class function Null: TSourceLocation; inline; static;

    { Retrieve the file, line, column, and offset represented by the source
      location.

      If the location refers into a macro expansion, retrieves the location of
      the macro expansion.

      Parameters:
        AFile: will be set to the file to which the given source location
          points.
        ALine: will be set to the line to which the given source location
          points.
        AColumn: will be set to the column to which the given source location
          points.
        AOffset: will be set to the offset into the buffer to which the given
          source location points. }
    procedure GetExpansionLocation(out AFile: TFile; out ALine, AColumn,
      AOffset: Integer); inline;

    (*Retrieve the file, line and column represented by the source location, as
      specified in a #line directive.

      Parameters:
        AFilename: will be set to the filename of the source location. Note that
          filenames returned will be for "virtual" files, which don't
          necessarily exist on the machine running clang - e.g. when parsing
          preprocessed output obtained from a different environment. For an
          invalid source location, an empty string is returned.
        ALine: will be set to the line number of the source location. For an
          invalid source location, zero is returned.
        AColumn: will be set to the column number of the source location. For an
          invalid source location, zero is returned.

      Example: given the following source code in a file somefile.c

      @preformatted(
        #123 "dummy.c" 1

        static int func(void)
        {
            return 0;
        }
      )

      the location information returned by this function would be

        File: dummy.c Line: 124 Column: 12

      whereas GetExpansionLocation would have returned

        File: somefile.c Line: 3 Column: 12 *)
    procedure GetPresumedLocation(out AFilename: String; out ALine,
      AColumn: Integer); inline;

    { Retrieve the file, line, column, and offset represented by the source
      location.

      If the location refers into a macro instantiation, return where the
      location was originally spelled in the source file.

      Parameters:
        AFile: will be set to the file to which the given source location
          points.
        ALine: will be set to the line to which the given source location
          points.
        AColumn: will be set to the column to which the given source location
          points.
        AOffset: will be set to the offset into the buffer to which the given
          source location points. }
    procedure GetSpellingLocation(out AFile: TFile; out ALine, AColumn,
      AOffset: Integer); inline;

    { Retrieve the file, line, column, and offset represented by the source
      location.

      If the location refers into a macro expansion, return where the macro was
      expanded or where the macro argument was written, if the location points
      at a macro argument.

      Parameters:
        AFile: will be set to the file to which the given source location
          points.
        ALine: will be set to the line to which the given source location
          points.
        AColumn: will be set to the column to which the given source location
          points.
        AOffset: will be set to the offset into the buffer to which the given
          source location points. }
    procedure GetFileLocation(out AFile: TFile; out ALine, AColumn,
      AOffset: Integer); inline;

    { Whether the source location is in a system header. }
    property IsInSystemHeader: Boolean read GetIsInSystemHeader;

    { Whether the source location is in the main file of the corresponding
      translation unit. }
    property IsFromMainFile: Boolean read GetIsFromMainFile;

    { Internal handle to C API }
    property Handle: TCXSourceLocation read FHandle;
  end;
  {$POINTERMATH ON}
  PSourceLocation = ^TSourceLocation;
  {$POINTERMATH OFF}

type
  { Identifies a half-open character range in the source code. }
  TSourceRange = record
  {$REGION 'Internal Declarations'}
  private
    FHandle: TCXSourceRange;
    function GetFirst: TSourceLocation; inline;
    function GetLast: TSourceLocation; inline;
  {$ENDREGION 'Internal Declarations'}
  public
    { Create a source range given the beginning and ending source locations. }
    constructor Create(const ABegin, AEnd: TSourceLocation);

    { Equality operators. Determine whether two ranges are equivalent. }
    class operator Equal(const ALeft, ARight: TSourceRange): Boolean; inline; static;
    class operator NotEqual(const ALeft, ARight: TSourceRange): Boolean; inline; static;

    { Whether the source range is assigned or not. }
    function IsNull: Boolean; inline;

    { A Null (invalid) source location }
    class function Null: TSourceRange; inline; static;

    { Source location representing the first character within a source range. }
    property First: TSourceLocation read GetFirst;

    { Source location representing the last character within a source range. }
    property Last: TSourceLocation read GetLast;

    { Internal handle to C API }
    property Handle: TCXSourceRange read FHandle;
  end;

type
  { A list of source ranges }
  ISourceRangeList = interface
  ['{60F53CC7-F1AE-48E7-8C79-A07499FAAED2}']
    {$REGION 'Internal Declarations'}
    function GetCount: Integer;
    function GetRange(const AIndex: Integer): TSourceRange;
    function GetHandle: PCXSourceRangeList;
    {$ENDREGION 'Internal Declarations'}

    { Number of ranges }
    property Count: Integer read GetCount;

    { Ranges }
    property Ranges[const AIndex: Integer]: TSourceRange read GetRange; default;

    { Internal handle to C API }
    property Handle: PCXSourceRangeList read GetHandle;
  end;

type
  { Provides the contents of a file that has not yet been saved to disk.

    Each TUnsavedFile instance provides the name of a file on the system along
    with the current contents of that file that have not yet been saved to
    disk. }
  TUnsavedFile = record
  public
    { The file whose contents have not yet been saved.
      This file must already exist in the file system. }
    Filename: String;

    { A buffer containing the unsaved contents of this file. }
    Contents: TBytes;
  end;

type
  { Information about a module. }
  TModule = record
  {$REGION 'Internal Declarations'}
  private
    FHandle: TCXModule;
    function GetAstFile: TFile; inline;
    function GetParent: TModule; inline;
    function GetName: String; inline;
    function GetFullName: String; inline;
    function GetIsSystem: Boolean; inline;
  {$ENDREGION 'Internal Declarations'}
  public
    { Whether the module is assigned or not. }
    function IsNull: Boolean; inline;

    { The module file where the provided module object came from. }
    property AstFile: TFile read GetAstFile;

    { The parent of a sub-module or Null if the given module is top-level, e.g.
      for 'std.vector' it will return the 'std' module. }
    property Parent: TModule read GetParent;

    { The name of the module, e.g. for the 'std.vector' sub-module it will
      return "vector". }
    property Name: String read GetName;

    { The full name of the module, e.g. "std.vector". }
    property FullName: String read GetFullName;

    { Whether this is a system module. }
    property IsSystem: Boolean read GetIsSystem;

    { Internal handle to C API }
    property Handle: TCXModule read FHandle;
  end;

type
  { A parsed comment. }
  TComment = record
  {$REGION 'Internal Declarations'}
  private
    FHandle: TCXComment;
    function GetKind: TCommentKind; inline;
    function GetChildCount: Integer; inline;
    function GetChild(const AIndex: Integer): TComment; inline;
    function GetIsWhitespace: Boolean; inline;
    function GetHasTrailingNewline: Boolean; inline;
    function GetText: String; inline;
    function GetInlineCommandName: String; inline;
    function GetInlineCommandRenderKind: TCommentInlineCommandRenderKind; inline;
    function GetInlineCommandArgCount: Integer; inline;
    function GetInlineCommandArg(const AIndex: Integer): String; inline;
    function GetHtmlTagName: String; inline;
    function GetHtmlTagIsSelfClosing: Boolean; inline;
    function GetHtmlTagAttrCount: Integer; inline;
    function GetHtmlTagAttrName(const AIndex: Integer): String; inline;
    function GetHtmlTagAttrValue(const AIndex: Integer): String; inline;
    function GetBlockCommandName: String; inline;
    function GetBlockCommandArgCount: Integer; inline;
    function GetBlockCommandArg(const AIndex: Integer): String; inline;
    function GetBlockCommandParagraph: TComment; inline;
    function GetParamCommandParamName: String; inline;
    function GetParamCommandIsParamIndexValid: Boolean; inline;
    function GetParamCommandParamIndex: Integer; inline;
    function GetParamCommandIsDirectionExplicit: Boolean; inline;
    function GetParamCommandDirection: TCommentParamPassDirection; inline;
    function GetTParamCommandParamName: String; inline;
    function GetTParamCommandIsParamPositionValied: Boolean; inline;
    function GetTParamCommandDepth: Integer; inline;
    function GetTParamCommandIndex(const ADepth: Integer): Integer; inline;
    function GetVerbatimBlockLineText: String; inline;
    function GetVerbatimLineText: String; inline;
    function GetHtmlTagAsString: String; inline;
    function GetFullCommentAsHtml: String; inline;
    function GetFullCommentASXml: String; inline;
  {$ENDREGION 'Internal Declarations'}
  public
    { Kind of comment }
    property Kind: TCommentKind read GetKind;

    { Number of comment children of this node }
    property ChildCount: Integer read GetChildCount;

    { Comment children of this node }
    property Children[const AIndex: Integer]: TComment read GetChild;

    { Whether this comment is whitespace. A Paragraph node is considered
      whitespace if it contains only Text nodes that are empty or whitespace.

      Other AST nodes (except Paragraph and Text) are never considered
      whitespace.}
    property IsWhitespace: Boolean read GetIsWhitespace;

    { Whether this is an inline content and has a newline immediately following
      it in the comment text. Newlines between paragraphs do not count.}
    property HasTrailingNewline: Boolean read GetHasTrailingNewline;

    { For Text AST nodes, the text of the comment }
    property Text: String read GetText;

    { For InlineCommand AST nodes, the name of the command }
    property InlineCommandName: String read GetInlineCommandName;

    { For InlineCommand AST nodes, the most appropriate rendering mode, chosen
      on command semantics in Doxygen. }
    property InlineCommandRenderKind: TCommentInlineCommandRenderKind read GetInlineCommandRenderKind;

    { For InlineCommand AST nodes, the number of command arguments }
    property InlineCommandArgCount: Integer read GetInlineCommandArgCount;

    { For InlineCommand AST nodes, the command arguments }
    property InlineCommandArgs[const AIndex: Integer]: String read GetInlineCommandArg;

    { For HtmlStartTag and HtmlEndTag AST nodes, the tag name }
    property HtmlTagName: String read GetHtmlTagName;

    { For HtmlStartTag AST nodes, whether the tag is self-closing (for example
      @code(<br />)) }
    property HtmlTagIsSelfClosing: Boolean read GetHtmlTagIsSelfClosing;

    { For HtmlStartTag AST nodes, the number of attributes (name-value pairs)
      attached to the tag. }
    property HtmlTagAttrCount: Integer read GetHtmlTagAttrCount;

    { For HtmlStartTag AST nodes, the attribute names. }
    property HtmlTagAttrNames[const AIndex: Integer]: String read GetHtmlTagAttrName;

    { For HtmlStartTag AST nodes, the attribute values. }
    property HtmlTagAttrValues[const AIndex: Integer]: String read GetHtmlTagAttrValue;

    { For BlockCommand AST nodes, the name of the block command. }
    property BlockCommandName: String read GetBlockCommandName;

    { For BlockCommand AST nodes, the number of word-like arguments }
    property BlockCommandArgCount: Integer read GetBlockCommandArgCount;

    { For BlockCommand AST nodes, the word-like arguments }
    property BlockCommandArgs[const AIndex: Integer]: String read GetBlockCommandArg;

    { For BlockCommand or VerbatimBlockCommand AST nodes, the paragraph argument
      of the block command }
    property BlockCommandParagraph: TComment read GetBlockCommandParagraph;

    { For ParamCommand AST nodes, the parameter name }
    property ParamCommandParamName: String read GetParamCommandParamName;

    { For ParamCommand AST nodes, whether the parameter that this AST node
      represents was found in the function prototype and ParamCommandParamIndex
      function will return a meaningful value. }
    property ParamCommandIsParamIndexValid: Boolean read GetParamCommandIsParamIndexValid;

    { For ParamCommand AST nodes, the zero-based parameter index in function
      prototype. }
    property ParamCommandParamIndex: Integer read GetParamCommandParamIndex;

    { For ParamCommand AST nodes, whether a parameter passing direction was
      specified explicitly in the comment. }
    property ParamCommandIsDirectionExplicit: Boolean read GetParamCommandIsDirectionExplicit;

    { For ParamCommand AST nodes, the parameter passing direction. }
    property ParamCommandDirection: TCommentParamPassDirection read GetParamCommandDirection;

    { For TParamCommand AST nodes, the template parameter name. }
    property TParamCommandParamName: String read GetTParamCommandParamName;

    { For TParamCommand AST nodes, whether the parameter that this AST node
      represents was found in the template parameter list and
      TParamCommandDepth and TParamCommandIndex functions will return meaningful
      values. }
    property TParamCommandIsParamPositionValied: Boolean read GetTParamCommandIsParamPositionValied;

    { For TParamCommand AST nodes, the zero-based nesting depth of this
      parameter in the template parameter list.

      For example,

      @preformatted(
        template<typename C, template<typename T> class TT>
        void test(TT<int> aaa);
      )

      For C and TT nesting depth is 0, for T nesting depth is 1. }
    property TParamCommandDepth: Integer read GetTParamCommandDepth;

    { For TParamCommand AST nodes, the zero-based parameter index in the
      template parameter list at a given nesting depth.

      For example,

      @preformatted(
        template<typename C, template<typename T> class TT>
        void test(TT<int> aaa);
      )

      For C and TT nesting depth is 0, so we can ask for index at depth 0:
      at depth 0 C's index is 0, TT's index is 1.

      For T nesting depth is 1, so we can ask for index at depth 0 and 1:
      at depth 0 T's index is 1 (same as TT's), at depth 1 T's index is 0. }
    property TParamCommandIndex[const ADepth: Integer]: Integer read GetTParamCommandIndex;

    { For VerbatimBlockLine AST nodes, the text contained in the AST node. }
    property VerbatimBlockLineText: String read GetVerbatimBlockLineText;

    { For VerbatimLine AST nodes, the text contained in the AST node. }
    property VerbatimLineText: String read GetVerbatimLineText;

    { For HtmlStartTag and HtmlEndTag AST nodes, the tag converted to a string }
    property HtmlTagAsString: String read GetHtmlTagAsString;

    { For FullComment AST nodes, the full parsed comment as an HTML fragment.

      Specific details of HTML layout are subject to change. Don't try to parse
      this HTML back into an AST, use other APIs instead.

      Currently the following CSS classes are used:
      * "para-brief" for \brief paragraph and equivalent commands;
      * "para-returns" for \returns paragraph and equivalent commands;
      * "word-returns" for the "Returns" word in \returns paragraph.

      Function argument documentation is rendered as a <dl\> list with arguments
      sorted in function prototype order. CSS classes used:
      * "param-name-index-NUMBER" for parameter name (<dt\>);
      * "param-descr-index-NUMBER" for parameter description (<dd\>);
      * "param-name-index-invalid" and "param-descr-index-invalid" are used if
        parameter index is invalid.

      Template parameter documentation is rendered as a <dl\> list with
      parameters sorted in template parameter list order. CSS classes used:
      * "tparam-name-index-NUMBER" for parameter name (<dt\>);
      * "tparam-descr-index-NUMBER" for parameter description (<dd\>);
      * "tparam-name-index-other" and "tparam-descr-index-other" are used for
        names inside template template parameters;
      * "tparam-name-index-invalid" and "tparam-descr-index-invalid" are used if
        parameter position is invalid. }
    property FullCommentAsHtml: String read GetFullCommentAsHtml;

    { For FullComment AST nodes, the full parsed comment as an XML document.

      A Relax NG schema for the XML can be found in comment-xml-schema.rng file
      inside clang source tree. }
    property FullCommentASXml: String read GetFullCommentASXml;

    { Internal handle to C API }
    property Handle: TCXComment read FHandle;
  end;

type
  { A semantic string that describes a code-completion result.

    A semantic string that describes the formatting of a code-completion result
    as a single "template" of text that should be inserted into the source
    buffer when a particular code-completion result is selected. Each semantic
    string is made up of some number of "chunks", each of which contains some
    text along with a description of what that text means, e.g., the name of the
    entity being referenced, whether the text chunk is part of the template, or
    whether it is a "placeholder" that the user should replace with actual code,
    of a specific kind. See TCompletionChunkKind for a description of the
    different kinds of chunks. }
  TCompletionString = record
  {$REGION 'Internal Declarations'}
  private
    FHandle: TCXCompletionString;
    function GetChunkCount: Integer; inline;
    function GetChunkKind(const AIndex: Integer): TCompletionChunkKind; inline;
    function GetChunkText(const AIndex: Integer): String; inline;
    function GetChunkCompletionString(const AIndex: Integer): TCompletionString; inline;
    function GetPriority: Integer; inline;
    function GetAvailability: TAvailabilityKind; inline;
    function GetAnnotationCount: Integer; inline;
    function GetAnnotation(const AIndex: Integer): String; inline;
    function GetParent: String; inline;
    function GetBriefComment: String; inline;
  {$ENDREGION 'Internal Declarations'}
  public
    { Whether the string is assigned or not. }
    function IsNull: Boolean; inline;

    { Number of chunks in the code-completion string. }
    property ChunkCount: Integer read GetChunkCount;

    { The kind of a particular chunk within the completion string. }
    property ChunkKind[const AIndex: Integer]: TCompletionChunkKind read GetChunkKind;

    { The text associated with a particular chunk within the completion string.}
    property ChunkText[const AIndex: Integer]: String read GetChunkText;

    { The completion string associated with a particular chunk within the
      completion string. }
    property ChunkCompletionString[const AIndex: Integer]: TCompletionString read GetChunkCompletionString;

    { The priority of this code completion.

      The priority of a code completion indicates how likely it is that this
      particular completion is the completion that the user will select. The
      priority is selected by various internal heuristics.

      Smaller values indicate higher-priority (more likely) completions. }
    property Priority: Integer read GetPriority;

    { The availability of the entity that this code-completion string refers to. }
    property Availability: TAvailabilityKind read GetAvailability;

    { The number of annotations associated with the completion string. }
    property AnnotationCount: Integer read GetAnnotationCount;

    { The annotations associated with the completion string. }
    property Annotations[const AIndex: Integer]: String read GetAnnotation;

    { The parent context of the completion string, e.g., "NSObject" if the
      completion string represents a method in the NSObject class.

      The parent context of a completion string is the semantic parent of the
      declaration (if any) that the code completion represents. For example, a
      code completion for an Objective-C method would have the method's class
      or protocol as its context. }
    property Parent: String read GetParent;

    { The brief documentation comment attached to the declaration that
      corresponds to the completion string. }
    property BriefComment: String read GetBriefComment;

    { Internal handle to C API }
    property Handle: TCXCompletionString read FHandle;
  end;

type
  { Evaluation result of a cursor }
  IEvalResult = interface
  ['{B7FB1524-EE99-4FCF-BD70-D8FEB39D8D64}']
    {$REGION 'Internal Declarations'}
    function GetKind: TEvalResultKind;
    function GetAsInt: Integer;
    function GetAsInt64: Int64;
    function GetIsUnsignedInt: Boolean;
    function GetAsUnsigned: UInt64;
    function GetAsDouble: Double;
    function GetAsString: String;
    function GetHandle: TCXEvalResult;
    {$ENDREGION 'Internal Declarations'}

    { The kind of the evaluated result. }
    property Kind: TEvalResultKind read GetKind;

    { The evaluation result as integer if the Kind is Int. }
    property AsInt: Integer read GetAsInt;

    { Returns the evaluation result as an Int64 if the Kind is Int. This
      prevents overflows that may happen if the result is returned with AsInt. }
    property AsInt64: Int64 read GetAsInt64;

    { Whether the Kind is Int and the evaluation result resulted in an unsigned
      integer. }
    property IsUnsignedInt: Boolean read GetIsUnsignedInt;

    { The evaluation result as an unsigned integer if the Kind is Int and
      IsUnsignedInt is True. }
    property AsUnsigned: UInt64 read GetAsUnsigned;

    { The evaluation result as double if the Kind is Float.}
    property AsDouble: Double read GetAsDouble;

    { The evaluation result as a constant string if the Kind is other than Int
      or Float. }
    property AsString: String read GetAsString;

    { Internal handle to C API }
    property Handle: TCXEvalResult read GetHandle;
  end;

type
  { A Unified Symbol Resolution (USR) is a string that identifies a particular
    entity (function, class, variable, etc.) within a program. USRs can be
    compared across translation units to determine, e.g., when references in
    one translation refer to an entity defined in another translation unit. }
  TUnifiedSymbolResolution = record
  {$REGION 'Internal Declarations'}
  private
    FHandle: TCXString;
  {$ENDREGION 'Internal Declarations'}
  public
    { Equality operators. Determine whether two URS's reference type same
      entity. }
    class operator Equal(const ALeft, ARight: TUnifiedSymbolResolution): Boolean; inline; static;
    class operator NotEqual(const ALeft, ARight: TUnifiedSymbolResolution): Boolean; inline; static;

    { Converts the USR to a string }
    function ToString: String; inline;

    { Internal handle to C API }
    property Handle: TCXString read FHandle;
  end;

type
  { Properties for the printing policy. }
  TPrintingPolicyProperty = (
    Indentation = CXPrintingPolicy_Indentation,
    SuppressSpecifiers = CXPrintingPolicy_SuppressSpecifiers,
    SuppressTagKeyword = CXPrintingPolicy_SuppressTagKeyword,
    IncludeTagDefinition = CXPrintingPolicy_IncludeTagDefinition,
    SuppressScope = CXPrintingPolicy_SuppressScope,
    SuppressUnwrittenScope = CXPrintingPolicy_SuppressUnwrittenScope,
    SuppressInitializers = CXPrintingPolicy_SuppressInitializers,
    ConstantArraySizeAsWritten = CXPrintingPolicy_ConstantArraySizeAsWritten,
    AnonymousTagLocations = CXPrintingPolicy_AnonymousTagLocations,
    SuppressStrongLifetime = CXPrintingPolicy_SuppressStrongLifetime,
    SuppressLifetimeQualifiers = CXPrintingPolicy_SuppressLifetimeQualifiers,
    SuppressTemplateArgsInCXXConstructors = CXPrintingPolicy_SuppressTemplateArgsInCXXConstructors,
    Bool = CXPrintingPolicy_Bool,
    Restrict = CXPrintingPolicy_Restrict,
    Alignof = CXPrintingPolicy_Alignof,
    UnderscoreAlignof = CXPrintingPolicy_UnderscoreAlignof,
    UseVoidForZeroParams = CXPrintingPolicy_UseVoidForZeroParams,
    TerseOutput = CXPrintingPolicy_TerseOutput,
    PolishForDeclaration = CXPrintingPolicy_PolishForDeclaration,
    Half = CXPrintingPolicy_Half,
    MSWChar = CXPrintingPolicy_MSWChar,
    IncludeNewlines = CXPrintingPolicy_IncludeNewlines,
    MSVCFormatting = CXPrintingPolicy_MSVCFormatting,
    ConstantsAsWritten = CXPrintingPolicy_ConstantsAsWritten,
    SuppressImplicitBase = CXPrintingPolicy_SuppressImplicitBase,
    FullyQualifiedName = CXPrintingPolicy_FullyQualifiedName,
    LastProperty = CXPrintingPolicy_LastProperty);

type
  { A policy that controls pretty printing for TCursor.PrettyPrinted }
  IPrintingPolicy = interface
  ['{AAEE40F0-049F-461A-B817-C9D7CC655A59}']
    {$REGION 'Internal Declarations'}
    function GetProperty(const AProp: TPrintingPolicyProperty): Integer;
    procedure SetProperty(const AProp: TPrintingPolicyProperty; const AValue: Integer);
    function GetHandle: TCXPrintingPolicy;
    {$ENDREGION 'Internal Declarations'}

    { Get or set property values }
    property Properties[const AProp: TPrintingPolicyProperty]: Integer read GetProperty write SetProperty;

    { Internal handle to C API }
    property Handle: TCXPrintingPolicy read GetHandle;
  end;

type
  { A cursor representing some element in the abstract syntax tree for
    a translation unit.

    The cursor abstraction unifies the different kinds of entities in a
    program--declaration, statements, expressions, references to declarations,
    etc.--under a single "cursor" abstraction with a common set of operations.
    Common operation for a cursor include: getting the physical location in
    a source file where the cursor points, getting the name associated with a
    cursor, and retrieving cursors for any child nodes of a particular cursor.

    Cursors can be produced in two specific ways.
    ITranslationUnit.Cursor produces a cursor for a translation unit, from which
    one can use VisitChildren to explore the rest of the translation unit.

    ITranslationUnit.GetCursor maps from a physical source location to the
    entity that resides at that location, allowing one to map from the
    source code into the AST. }
  TCursor = record
  {$REGION 'Internal Declarations'}
  private
    FHandle: TCXCursor;
    function GetKind: TCursorKind; inline;
    function GetIsNull: Boolean; inline;
    function GetIsDeclaration: Boolean; inline;
    function GetIsInvalidDeclaration: Boolean; inline;
    function GetIsReference: Boolean; inline;
    function GetIsExpression: Boolean; inline;
    function GetIsStatement: Boolean; inline;
    function GetIsAttribute: Boolean; inline;
    function GetHasAttributes: Boolean; inline;
    function GetIsInvalid: Boolean; inline;
    function GetIsTranslationUnit: Boolean; inline;
    function GetIsPreprocessing: Boolean; inline;
    function GetIsUnexposed: Boolean; inline;
    function GetLinkage: TLinkageKind; inline;
    function GetVisibility: TVisibility; inline;
    function GetAvailability: TAvailabilityKind; inline;
    function GetLanguage: TLanguageKind; inline;
    function GetTlsKind: TTlsKind; inline;
    function GetSemanticParent: TCursor; inline;
    function GetLexicalParent: TCursor; inline;
    function GetIncludedFile: TFile; inline;
    function GetLocation: TSourceLocation; inline;
    function GetExtent: TSourceRange; inline;
    function GetCursorType: TType; inline;
    function GetTypedefDeclUnderlyingType: TType; inline;
    function GetEnumDeclIntegerType: TType; inline;
    function GetEnumConstantDeclValue: Int64; inline;
    function GetEnumConstantDeclUnsignedValue: UInt64; inline;
    function GetFieldDeclBitWidth: Integer; inline;
    function GetArgumentCount: Integer; inline;
    function GetArgument(const AIndex: Integer): TCursor; inline;
    function GetTemplateArgumentCount: Integer; inline;
    function GetTemplateArgumentKind(const AIndex: Integer): TTemplateArgumentKind; inline;
    function GetTemplateArgumentType(const AIndex: Integer): TType; inline;
    function GetTemplateArgumentValue(const AIndex: Integer): Int64; inline;
    function GetTemplateArgumentUnsignedValue(const AIndex: Integer): UInt64; inline;
    function GetIsMacroFunctionLike: Boolean; inline;
    function GetIsMacroBuiltin: Boolean; inline;
    function GetIsFunctionInlined: Boolean; inline;
    function GetDeclObjCTypeEncoding: String; inline;
    function GetResultType: TType; inline;
    function GetExceptionSpecificationType: TExceptionSpecificationKind; inline;
    function GetOffsetOfField: Int64; inline;
    function GetIsAnonymous: Boolean; inline;
    function GetIsBitField: Boolean; inline;
    function GetIsVirtualBase: Boolean; inline;
    function GetCxxAccessSpecifier: TCxxAccessSpecifier; inline;
    function GetStorageClass: TStorageClass; inline;
    function GetOverloadedDeclCount: Integer; inline;
    function GetOverloadedDecl(const AIndex: Integer): TCursor; inline;
    function GetIBOutletCollectionType: TType; inline;
    function GetUsr: TUnifiedSymbolResolution; inline;
    function GetSpelling: String; inline;
    function GetDisplayName: String; inline;
    function GetReferenced: TCursor; inline;
    function GetDefinition: TCursor; inline;
    function GetIsDefinition: Boolean; inline;
    function GetCanonical: TCursor; inline;
    function GetObjCSelectorIndex: Integer; inline;
    function GetIsDynamicCall: Boolean; inline;
    function GetReceiverType: TType; inline;
    function GetObjCPropertyAttributes: TObjCPropertyAttrKinds; inline;
    function GetObjCDeclQualifiers: TObjCDeclQualifierKinds; inline;
    function GetIsObjCOptional: Boolean; inline;
    function GetIsVariadic: Boolean; inline;
    function GetCommentRange: TSourceRange; inline;
    function GetRawComment: String; inline;
    function GetBriefComment: String; inline;
    function GetMangling: String; inline;
    function GetCxxManglings: TArray<String>;
    function GetObjCManglings: TArray<String>;
    function GetModule: TModule; inline;
    function GetCxxConstructorIsConvertingConstructor: Boolean; inline;
    function GetCxxConstructorIsCopyConstructor: Boolean; inline;
    function GetCxxConstructorIsDefaultConstructor: Boolean; inline;
    function GetCxxConstructorIsMoveConstructor: Boolean; inline;
    function GetCxxFieldIsMutable: Boolean; inline;
    function GetCxxMethodIsDefaulted: Boolean; inline;
    function GetCxxMethodIsPureVirtual: Boolean; inline;
    function GetCxxMethodIsStatic: Boolean; inline;
    function GetCxxMethodIsVirtual: Boolean; inline;
    function GetCxxRecordIsAbstract: Boolean; inline;
    function GetEnumDeclIsScoped: Boolean; inline;
    function GetCxxMethodIsConst: Boolean; inline;
    function GetTemplateCursorKind: TCursorKind; inline;
    function GetSpecializedCursorTemplate: TCursor; inline;
    function GetCompletionString: TCompletionString; inline;
    function GetParsedComment: TComment; inline;
    function GetIsAnonymousRecordDecl: Boolean; inline;
    function GetIsInlineNamespace: Boolean; inline;
    function GetPrintingPolicy: IPrintingPolicy;
    function GetObjCPropertyGetterName: String; inline;
    function GetObjCPropertySetterName: String; inline;
  private
    class function GetNull: TCursor; inline; static;
  {$ENDREGION 'Internal Declarations'}
  public
    { Determine whether two cursors are equivalent. }
    class operator Equal(const ALeft, ARight: TCursor): Boolean; inline; static;
    class operator NotEqual(const ALeft, ARight: TCursor): Boolean; inline; static;

    { Compute a hash value for the given cursor. }
    function Hash: Cardinal; inline;

    { Determine the availability of the entity that this cursor refers to
      on any platforms for which availability information is known.

      Returns:
        Global and platform-specific availibility }
    function GetPlatformAvailability: TPlatformAvailability;

    { Determine the set of methods that are overridden by the given method.

      Returns:
        List of cursors, representing the set of overridden methods, or nil if
        there are no overridden methods.

      In both Objective-C and C++, a method (aka virtual member function, in
      C++) can override a virtual method in a base class. For Objective-C, a
      method is said to override any method in the class's base class, its
      protocols, or its categories' protocols, that has the same selector and is
      of the same kind (class or instance). If no such method exists, the search
      continues to the class's superclass, its protocols, and its categories,
      and so on. A method from an Objective-C implementation is considered to
      override the same methods as its corresponding method in the interface.

      For C++, a virtual member function overrides any virtual member function
      with the same signature that occurs in its base classes. With multiple
      inheritance, a virtual member function can override several virtual member
      functions coming from different base classes.

      In all cases, this function determines the immediate overridden method,
      rather than all of the overridden methods. For example, if a method is
      originally declared in a class A, then overridden in B (which in inherits
      from A) and also in C (which inherited from B), then the only overridden
      method returned from this function when invoked on C's method will be B's
      method. The client may then invoke this function again, given the
      previously-found overridden methods, to map out the complete
      method-override set. }
    function GetOverriddenCursors: TArray<TCursor>;

    { Retrieve a range for a piece that forms the cursors spelling name.

      Parameters:
        APieceIndex:  the index of the spelling name piece.

      Returns:
        The source range. If APieceIndex is greater than the actual number of
        pieces, it will return a NullL (invalid) range.

      Most of the times there is only one range for the complete spelling but
      for Objective-C methods and Objective-C message expressions, there are
      multiple pieces for each selector identifier. }
    function GetSpellingNameRange(const APieceIndex: Integer): TSourceRange;

    { Returns True if the cursor points to a symbol marked with
      external_source_symbol attribute.

      Parameters:
        ALanguage: if the attribute is present, will be set to the 'language'
          string from the attribute.
        ADefinedIn: if the attribute is present, will be set to the 'definedIn'
          string from the attribute.
        AIsGenerated: if the attribute is present, will be set to True if the
          'generated_declaration' is set in the attribute.

      Returns:
        True if the cursor points to a symbol marked with
        external_source_symbol attribute.}
    function IsExternalSymbol(out ALanguage: String; out ADefinedIn: String;
      out AIsGenerated: Boolean): Boolean;

    { Given a cursor that references something else, return the source range
      covering that reference.

      Parameters:
        AFlags: configuration flags.
        APieceIndex: for contiguous names or when passing the flag
          WantSinglePiece, only one piece with index 0 is available. When the
          WantSinglePiece flag is not passed for a non-contiguous names, this
          index can be used to retrieve the individual pieces of the name.

      Returns:
        The piece of the name pointed to by the given cursor. If there is no
        name, or if APieceIndex is out-of-range, a Null-cursor will be
        returned. }
    function GetReferenceNameRange(const AFlags: TNameRefFlags;
      const APieceIndex: Integer): TSourceRange; inline;

    { If cursor is a statement declaration tries to evaluate the statement and
      if its variable, tries to evaluate its initializer, into its corresponding
      type. }
    function Evaluate: IEvalResult;

    { Pretty print declarations.

      Parameters:
        APolicy: (optional) policy to control the entities being printed.
          If nil, a default policy is used.

      Returns:
        The pretty printed declaration or the empty string for other cursors. }
    function PrettyPrinted(const APolicy: IPrintingPolicy = nil): String; inline;

    { The Null cursor, which represents no entity. }
    class property Null: TCursor read GetNull;

    { The kind of the cursor. }
    property Kind: TCursorKind read GetKind;

    { True if this is a Null cursor. }
    property IsNull: Boolean read GetIsNull;

    { Whether the cursor represents a declaration. }
    property IsDeclaration: Boolean read GetIsDeclaration;

    { Whether the given declaration is invalid.

      A declaration is invalid if it could not be parsed successfully. }
    property IsInvalidDeclaration: Boolean read GetIsInvalidDeclaration;

    { Whether the cursor represents a simple reference.

      Note that other kinds of cursors (such as expressions) can also refer to
      other cursors. Use CursorReferenced to determine whether a particular
      cursor refers to another entity. }
    property IsReference: Boolean read GetIsReference;

    { Whether the cursor represents an expression. }
    property IsExpression: Boolean read GetIsExpression;

    { Whether the cursor represents a statement. }
    property IsStatement: Boolean read GetIsStatement;

    { Whether the cursor represents an attribute. }
    property IsAttribute: Boolean read GetIsAttribute;

    { Whether the cursor has any attributes. }
    property HasAttributes: Boolean read GetHasAttributes;

    { Whether the cursor represents an invalid cursor. }
    property IsInvalid: Boolean read GetIsInvalid;

    { Whether the cursor represents a translation unit. }
    property IsTranslationUnit: Boolean read GetIsTranslationUnit;

    { Whether the cursor represents a preprocessing element, such as a
      preprocessor directive or macro instantiation. }
    property IsPreprocessing: Boolean read GetIsPreprocessing;

    { Determine whether the cursor represents a currently unexposed piece of the
      AST (e.g., TCursorKind.UnexposedStmt). }
    property IsUnexposed: Boolean read GetIsUnexposed;

    { The linkage of the entity referred to by the cursor. }
    property Linkage: TLinkageKind read GetLinkage;

    { The visibility of the entity referred to by a cursor.

      This returns the default visibility if not explicitly specified by a
      visibility attribute. The default visibility may be changed by commandline
      arguments. }
    property Visibility: TVisibility read GetVisibility;

    { The availability of the entity that this cursor refers to, taking the
      current target platform into account. }
    property Availability: TAvailabilityKind read GetAvailability;

    { The "language" of the entity referred to by the cursor. }
    property Language: TLanguageKind read GetLanguage;

    { The "thread-local storage (TLS) kind" of the declaration referred to by
      the cursor. }
    property TlsKind: TTlsKind read GetTlsKind;

    (*The semantic parent of the given cursor.

      The semantic parent of a cursor is the cursor that semantically contains
      the given cursor. For many declarations, the lexical and semantic parents
      are equivalent (the lexical parent is returned by LexicalParent). They
      diverge when declarations or definitions are provided out-of-line. For
      example:

      @preformatted(
        class C {
         void f();
        };
        void C::f() { }
      )

      In the out-of-line definition of C::f, the semantic parent is the class C,
      of which this function is a member. The lexical parent is the place where
      the declaration actually occurs in the source code; in this case, the
      definition occurs in the translation unit. In general, the lexical parent
      for a given entity can change without affecting the semantics of the
      program, and the lexical parent of different declarations of the same
      entity may be different. Changing the semantic parent of a declaration, on
      the other hand, can have a major impact on semantics, and redeclarations
      of a particular entity should all have the same semantic context.

      In the example above, both declarations of C::f have C as their semantic
      context, while the lexical context of the first C::f is C and the lexical
      context of the second C::f is the translation unit.

      For global declarations, the semantic parent is the translation unit.*)
    property SemanticParent: TCursor read GetSemanticParent;

    (*The lexical parent of the given cursor.

      The lexical parent of a cursor is the cursor in which the given cursor
      was actually written. For many declarations, the lexical and semantic
      parents are equivalent (the semantic parent is returned by
      SemanticParent). They diverge when declarations or definitions are
      provided out-of-line. For example:

      @preformatted(
        class C {
         void f();
        };

        void C::f() { }
      )

      In the out-of-line definition of C::f, the semantic parent is the class C,
      of which this function is a member. The lexical parent is the place where
      the declaration actually occurs in the source code; in this case, the
      definition occurs in the translation unit. In general, the lexical parent
      for a given entity can change without affecting the semantics of the
      program, and the lexical parent of different declarations of the same
      entity may be different. Changing the semantic parent of a declaration, on
      the other hand, can have a major impact on semantics, and redeclarations
      of a particular entity should all have the same semantic context.

      In the example above, both declarations of C::f have C as their semantic
      context, while the lexical context of the first C::f is C and the lexical
      context of the second C::f is the translation unit.

      For declarations written in the global scope, the lexical parent is the
      translation unit.*)
    property LexicalParent: TCursor read GetLexicalParent;

    { The file that is included by the given inclusion directive cursor. }
    property IncludedFile: TFile read GetIncludedFile;

    { The physical location of the source constructor referenced by the cursor.

      The location of a declaration is typically the location of the name of
      that declaration, where the name of that declaration would occur if it is
      unnamed, or some keyword that introduces that particular declaration.
      The location of a reference is where that reference occurs within the
      source code. }
    property Location: TSourceLocation read GetLocation;

    { The physical extent of the source construct referenced by the cursor.

      The extent of a cursor starts with the file/line/column pointing at the
      first character within the source construct that the cursor refers to and
      ends with the last character within that source construct. For a
      declaration, the extent covers the declaration itself. For a reference,
      the extent covers the location of the reference (e.g., where the
      referenced entity was actually used). }
    property Extent: TSourceRange read GetExtent;

    { The type of the Cursor (if any). }
    property CursorType: TType read GetCursorType;

    { The underlying type of a typedef declaration.

      If the cursor does not reference a typedef declaration, an invalid type is
      returned. }
    property TypedefDeclUnderlyingType: TType read GetTypedefDeclUnderlyingType;

    { The integer type of an enum declaration.

      If the cursor does not reference an enum declaration, an invalid type is
      returned.}
    property EnumDeclIntegerType: TType read GetEnumDeclIntegerType;

    { The integer value of an enum constant declaration as a signed Int64.

      If the cursor does not reference an enum constant declaration,
      Int64.MinValue is returned. Since this is also potentially a valid
      constant value, the kind of the cursor must be verified before calling
      this function. }
    property EnumConstantDeclValue: Int64 read GetEnumConstantDeclValue;

    { The integer value of an enum constant declaration as an UInt64.

      If the cursor does not reference an enum constant declaration,
      UInt64.MaxValue is returned. Since this is also potentially a valid
      constant value, the kind of the cursor must be verified before calling
      this function. }
    property EnumConstantDeclUnsignedValue: UInt64 read GetEnumConstantDeclUnsignedValue;

    { The bit width of a bit field declaration as an integer.
      If a cursor that is not a bit field declaration is passed in, -1 is
      returned. }
    property FieldDeclBitWidth: Integer read GetFieldDeclBitWidth;

    { The number of non-variadic arguments associated with the cursor.

      The number of arguments can be determined for calls as well as for
      declarations of functions or methods. For other cursors -1 is returned. }
    property ArgumentCount: Integer read GetArgumentCount;

    { The argument cursor of a function or method.

      The argument cursor can be determined for calls as well as for
      declarations of functions or methods. For other cursors and for invalid
      indices, an invalid cursor is returned. }
    property Arguments[const AIndex: Integer]: TCursor read GetArgument;

    (*The number of template args of a function decl representing a template
      specialization.

      If the argument cursor cannot be converted into a template function
      declaration, -1 is returned.

      For example, for the following declaration and specialization:

      @preformatted(
        template <typename T, int kInt, bool kBool>
        void foo() { ... }

        template <>
        void foo<float, -7, true>();
      )

      The value 3 would be returned from this property.*)
    property TemplateArgumentCount: Integer read GetTemplateArgumentCount;

    (*The kind of the I'th template argument of the cursor.

      If the argument cursor does not represent a FunctionDecl, an invalid
      template argument kind is returned.

      For example, for the following declaration and specialization:

      @preformatted(
        template <typename T, int kInt, bool kBool>
        void foo() { ... }

        template <>
        void foo<float, -7, true>();
      )

      For I = 0, 1, and 2, Type, Integral, and Integral will be returned,
      respectively.*)
    property TemplateArgumentKind[const AIndex: Integer]: TTemplateArgumentKind read GetTemplateArgumentKind;

    (*The type of a TemplateArgument of a function decl representing a template
      specialization.

      If the argument cursor does not represent a FunctionDecl whose I'th
      template argument has a kind of CXTemplateArgKind_Integral, an invalid
      type is returned.

      For example, for the following declaration and specialization:

      @preformatted(
        template <typename T, int kInt, bool kBool>
        void foo() { ... }

        template <>
        void foo<float, -7, true>();
      )

      If called with I = 0, "float", will be returned. Invalid types will be
      returned for I = 1 or 2.*)
    property TemplateArgumentType[const AIndex: Integer]: TType read GetTemplateArgumentType;

    (*The value of an Integral TemplateArgument (of a function decl representing
      a template specialization) as a signed Int64.

      It is undefined to call this function on a cursor that does not represent
      a FunctionDecl or whose I'th template argument is not an integral value.

      For example, for the following declaration and specialization:

      @preformatted(
        template <typename T, int kInt, bool kBool>
        void foo() { ... }

        template <>
        void foo<float, -7, true>();
      )

      If called with I = 1 or 2, -7 or true will be returned, respectively.
      For I = 0, this function's behavior is undefined.*)
    property TemplateArgumentValue[const AIndex: Integer]: Int64 read GetTemplateArgumentValue;

    (*The value of an Integral TemplateArgument (of a function decl representing
      a template specialization) as an UInt64.

      It is undefined to call this function on a cursor that does not represent
      a FunctionDecl or whose I'th template argument is not an integral value.

      For example, for the following declaration and specialization:

      @preformatted(
        template <typename T, int kInt, bool kBool>
        void foo() { ... }

        template <>
        void foo<float, 2147483649, true>();
      )

      If called with I = 1 or 2, 2147483649 or true will be returned,
      respectively. For I = 0, this function's behavior is undefined.*)
    property TemplateArgumentUnsignedValue[const AIndex: Integer]: UInt64 read GetTemplateArgumentUnsignedValue;

    { Whether the cursor that is a macro, is function like. }
    property IsMacroFunctionLike: Boolean read GetIsMacroFunctionLike;

    { Whether the cursor that is a macro, is a builtin one. }
    property IsMacroBuiltin: Boolean read GetIsMacroBuiltin;

    { Whether the cursor that is a function declaration, is an inline
      declaration. }
    property IsFunctionInlined: Boolean read GetIsFunctionInlined;

    { The Objective-C type encoding for the specified declaration. }
    property DeclObjCTypeEncoding: String read GetDeclObjCTypeEncoding;

    { The return type associated with a given cursor.

      This only returns a valid type if the cursor refers to a function or
      method. }
    property ResultType: TType read GetResultType;

    { The exception specification type associated with a given cursor.

      This only returns a valid result if the cursor refers to a function or
      method. }
    property ExceptionSpecificationType: TExceptionSpecificationKind read GetExceptionSpecificationType;

    { The offset of the field represented by the Cursor.

      * If the cursor is not a field declaration, -1 is returned.
      * If the cursor semantic parent is not a record field declaration,
        TYPE_LAYOUT_ERROR_INVALID is returned.
      * If the field's type declaration is an incomplete type,
        TYPE_LAYOUT_ERROR_INCOMPLETE is returned.
      * If the field's type declaration is a dependent type,
        TYPE_LAYOUT_ERROR_DEPENDENT is returned.
      * If the field's name is not found, TYPE_LAYOUT_ERROR_INVALID_FIELDNAME
        is returned. }
    property OffsetOfField: Int64 read GetOffsetOfField;

    { Whether the cursor represents an anonymous tag or namespace.}
    property IsAnonymous: Boolean read GetIsAnonymous;

    { Whether the cursor represents an anonymous record declaration. }
    property IsAnonymousRecordDecl: Boolean read GetIsAnonymousRecordDecl;

    { Whether the cursor represents an inline namespace declaration. }
    property IsInlineNamespace: Boolean read GetIsInlineNamespace;

    { Whether the cursor specifies a Record member that is a bitfield. }
    property IsBitField: Boolean read GetIsBitField;

    { Whether the base class specified by the cursor with kind CXXBaseSpecifier
      is virtual. }
    property IsVirtualBase: Boolean read GetIsVirtualBase;

    { The access control level for the referenced object.

      If the cursor refers to a C++ declaration, its access control level within
      its parent scope is returned. Otherwise, if the cursor refers to a base
      specifier or access specifier, the specifier itself is returned. }
    property CxxAccessSpecifier: TCxxAccessSpecifier read GetCxxAccessSpecifier;

    { The storage class for a function or variable declaration.

      If the cursor is not a function or variable declaration, Invalid is
      returned else the storage class. }
    property StorageClass: TStorageClass read GetStorageClass;

    { The number of overloaded declarations referenced by a OverloadedDeclRef
      cursor. If it is not a OverloadedDeclRef cursor, returns 0. }
    property OverloadedDeclCount: Integer read GetOverloadedDeclCount;

    { Retrieve a cursor for one of the overloaded declarations referenced
      by a OverloadedDeclRef cursor.

      If the cursor does not have an associated set of overloaded declarations,
      or if the index is out of bounds, TCursor.Null. }
    property OverloadedDecls[const AIndex: Integer]: TCursor read GetOverloadedDecl;

    { For cursors representing an iboutletcollection attribute, this property
      returns the collection element type. }
    property IBOutletCollectionType: TType read GetIBOutletCollectionType;

    { The Unified Symbol Resolution (USR) for the entity referenced by the given
      cursor.

      A Unified Symbol Resolution (USR) is a string that identifies a particular
      entity (function, class, variable, etc.) within a program. USRs can be
      compared across translation units to determine, e.g., when references in
      one translation refer to an entity defined in another translation unit. }
    property Usr: TUnifiedSymbolResolution read GetUsr;

    { The name for the entity referenced by this cursor. }
    property Spelling: String read GetSpelling;

    { The display name for the entity referenced by this cursor.

      The display name contains extra information that helps identify the
      cursor, such as the parameters of a function or template or the arguments
      of a class template specialization. }
    property DisplayName: String read GetDisplayName;

    { For a cursor that is a reference, retrieve a cursor representing the
      entity that it references.

      Reference cursors refer to other entities in the AST. For example, an
      Objective-C superclass reference cursor refers to an Objective-C class.
      This function produces the cursor for the Objective-C class from the
      cursor for the superclass reference. If the input cursor is a declaration
      or definition, it returns that declaration or definition unchanged.
      Otherwise, returns the Null cursor. }
    property Referenced: TCursor read GetReferenced;

    (*For a cursor that is either a reference to or a declaration of some
      entity, retrieve a cursor that describes the definition of that entity.

      Some entities can be declared multiple times within a translation unit,
      but only one of those declarations can also be a definition. For example,
      given:

      @preformatted(
        int f(int, int);
        int g(int x, int y) { return f(x, y); }
        int f(int a, int b) { return a + b; }
        int f(int, int);
      )

      there are three declarations of the function "f", but only the second one
      is a definition. The Definition property will take any cursor pointing to
      a declaration of "f" (the first or fourth lines of the example) or a
      cursor referenced that uses "f" (the call to "f' inside "g") and will
      return a declaration cursor pointing to the definition (the second "f"
      declaration).

      If given a cursor for which there is no corresponding definition, e.g.,
      because there is no definition of that entity within this translation
      unit, returns a Null cursor.*)
    property Definition: TCursor read GetDefinition;

    { Whether the declaration pointed to by this cursor is also a definition of
      that entity. }
    property IsDefinition: Boolean read GetIsDefinition;

    (*The canonical cursor corresponding to the given cursor.

      In the C family of languages, many kinds of entities can be declared
      several times within a single translation unit. For example, a structure
      type can be forward-declared (possibly multiple times) and later defined:

      @preformatted(
        struct X;
        struct X;
        struct X {
          int member;
        };
      )

      The declarations and the definition of X are represented by three
      different cursors, all of which are declarations of the same underlying
      entity. One of these cursor is considered the "canonical" cursor, which
      is effectively the representative for the underlying entity. One can
      determine if two cursors are declarations of the same underlying entity by
      comparing their canonical cursors.*)
    property Canonical: TCursor read GetCanonical;

    { If the cursor points to a selector identifier in an Objective-C method or
      message expression, this returns the selector index, otherwise it returns
      -1.

      After getting a curso, this can be called to determine if the location
      points to a selector identifier. }
    property ObjCSelectorIndex: Integer read GetObjCSelectorIndex;

    { Given a cursor pointing to a C++ method call or an Objective-C message,
      returns True if the method/message is "dynamic", meaning:

      For a C++ method: the call is virtual.
      For an Objective-C message: the receiver is an object instance, not
      'super' or a specific class.

      If the method/message is "static" or the cursor does not point to a
      method/message, it will return False. }
    property IsDynamicCall: Boolean read GetIsDynamicCall;

    { Given a cursor pointing to an Objective-C message or property reference,
      or C++ method call, returns the TType of the receiver. }
    property ReceiverType: TType read GetReceiverType;

    { Given a cursor that represents a property declaration, return the
      associated property attributes. }
    property ObjCPropertyAttributes: TObjCPropertyAttrKinds read GetObjCPropertyAttributes;

    { Given a cursor that represents a property declaration, return the
      name of the method that implements the getter. }
    property ObjCPropertyGetterName: String read GetObjCPropertyGetterName;

    { Given a cursor that represents a property declaration, return the
      name of the method that implements the setter, if any. }
    property ObjCPropertySetterName: String read GetObjCPropertySetterName;

    { Given a cursor that represents an Objective-C method or parameter
      declaration, return the associated Objective-C qualifiers for the return
      type or the parameter respectively. }
    property ObjCDeclQualifiers: TObjCDeclQualifierKinds read GetObjCDeclQualifiers;

    { Given a cursor that represents an Objective-C method or property
      declaration, return True if the declaration was affected by "@@optional".
      Returns False if the cursor is not such a declaration or it is
      "@@required".}
    property IsObjCOptional: Boolean read GetIsObjCOptional;

    { True if the cursor is a variadic function or method. }
    property IsVariadic: Boolean read GetIsVariadic;

    { Given a cursor that represents a declaration, return the associated
      comment's source range. The range may include multiple consecutive
      comments with whitespace in between. }
    property CommentRange: TSourceRange read GetCommentRange;

    { Given a cursor that represents a declaration, return the associated
      comment text, including comment markers. }
    property RawComment: String read GetRawComment;

    { Given a cursor that represents a documentable entity (e.g., declaration),
      return the associated "brief" paragraph; otherwise return the first
      paragraph.}
    property BriefComment: String read GetBriefComment;

    { The mangled name of the cursor. }
    property Mangling: String read GetMangling;

    { The mangled symbols of the C++ constructor or destructor at the cursor. }
    property CxxManglings: TArray<String> read GetCxxManglings;

    { The mangled symbols of the ObjC class interface or implementation at the
      cursor. }
    property ObjCManglings: TArray<String> read GetObjCManglings;

    { Given a ModuleImportDecl cursor, return the associated module. }
    property Module: TModule read GetModule;

    { Whether a C++ constructor is a converting constructor. }
    property CxxConstructorIsConvertingConstructor: Boolean read GetCxxConstructorIsConvertingConstructor;

    { Whether a C++ constructor is a copy constructor. }
    property CxxConstructorIsCopyConstructor: Boolean read GetCxxConstructorIsCopyConstructor;

    { Whether a C++ constructor is the default constructor. }
    property CxxConstructorIsDefaultConstructor: Boolean read GetCxxConstructorIsDefaultConstructor;

    { Whether a C++ constructor is a move constructor. }
    property CxxConstructorIsMoveConstructor: Boolean read GetCxxConstructorIsMoveConstructor;

    { Whether a C++ field is declared 'mutable'. }
    property CxxFieldIsMutable: Boolean read GetCxxFieldIsMutable;

    { Whether a C++ method is declared '= default'. }
    property CxxMethodIsDefaulted: Boolean read GetCxxMethodIsDefaulted;

    { Whether a C++ member function or member function template is pure
      virtual. }
    property CxxMethodIsPureVirtual: Boolean read GetCxxMethodIsPureVirtual;

    { Whether a C++ member function or member function template is declared
      'static'.}
    property CxxMethodIsStatic: Boolean read GetCxxMethodIsStatic;

    { Whether a C++ member function or member function template is explicitly
      declared 'virtual' or if it overrides a virtual method from one of the
      base classes. }
    property CxxMethodIsVirtual: Boolean read GetCxxMethodIsVirtual;

    { Whether a C++ record is abstract, i.e. whether a class or struct has
      a pure virtual member function. }
    property CxxRecordIsAbstract: Boolean read GetCxxRecordIsAbstract;

    { Whether an enum declaration refers to a scoped enum. }
    property EnumDeclIsScoped: Boolean read GetEnumDeclIsScoped;

    { Whether a C++ member function or member function template is declared
      'const'. }
    property CxxMethodIsConst: Boolean read GetCxxMethodIsConst;

    { Given a cursor that represents a template, determine the cursor kind of
      the specializations would be generated by instantiating the template.

      This property can be used to determine what flavor of function template,
      class template, or class template partial specialization is stored in the
      cursor. For example, it can describe whether a class template cursor is
      declared with "struct", "class" or "union".

      If the cursor is not a template, returns NoDeclFound. }
    property TemplateCursorKind: TCursorKind read GetTemplateCursorKind;

    { Given a cursor that may represent a specialization or instantiation of a
      template, retrieve the cursor that represents the template that it
      specializes or from which it was instantiated.

      This routine determines the template involved both for explicit
      specializations of templates and for implicit instantiations of the
      template, both of which are referred to as "specializations". For a class
      template specialization (e.g., std::vector<bool>), this routine will
      return either the primary template (std::vector) or, if the specialization
      was instantiated from a class template partial specialization, the class
      template partial specialization. For a class template partial
      specialization and a function template specialization (including
      instantiations), this this routine will return the specialized template.

      For members of a class template (e.g., member functions, member classes,
      or static data members), returns the specialized or instantiated member.
      Although not strictly "templates" in the C++ language, members of class
      templates have the same notions of specializations and instantiations that
      templates do, so this routine treats them similarly.

      If the cursor is NOT a specialization or instantiation of a template or a
      member thereof, returns a Null cursor. }
    property SpecializedCursorTemplate: TCursor read GetSpecializedCursorTemplate;

    { A non-context-sensitive completion string for declaration and macro
      definition cursors, or a Null string for other kinds of cursors.}
    property CompletionString: TCompletionString read GetCompletionString;

    { Given a cursor that represents a documentable entity (e.g., declaration),
      return the associated parsed comment as a FullComment AST node. }
    property ParsedComment: TComment read GetParsedComment;

    { The default policy for the cursor.
      Can be used with PrettyPrinted. }
    property PrintingPolicy: IPrintingPolicy read GetPrintingPolicy;

    { Internal handle to C API }
    property Handle: TCXCursor read FHandle;
  end;

  { Visitor invoked for each cursor found by a traversal.

    This visitor function will be invoked for each cursor found by
    TCursor.VisitChildren. Its first argument is the cursor being visited, its
    second argument is the parent visitor for that cursor.

    The visitor should return one of the TChildVisitResult values to direct
    TCursor.VisitChildren. }
  TCursorVisitor = reference to function(const ACursor, AParent: TCursor): TChildVisitResult;
  TCursorVisitorMethod = function(const ACursor, AParent: TCursor): TChildVisitResult of object;

  { Visitor that receives a cursor and source range }
  TCursorAndRangeVisitor = reference to function(const ACursor: TCursor;
    const ARange: TSourceRange): TVisitResult;
  TCursorAndRangeVisitorMethod = function(const ACursor: TCursor;
    const ARange: TSourceRange): TVisitResult of object;

  { Extends TCursor }
  TCursorHelper = record helper for TCursor
  {$REGION 'Internal Declarations'}
  private
    class function CursorVisitor(ACursor: TCXCursor; AParent: TCXCursor;
      AClientData: TCXClientData): TCXChildVisitResult; cdecl; static;
  {$ENDREGION 'Internal Declarations'}
  public
    { Visit the children of this cursor.

      Parameters:
        AVisitor: the visitor function that will be invoked for each child of
          this cursor.

      Returns:
        True if the traversal was terminated prematurely by the visitor
        returning TChildVisitResult.Break.

      This function visits all the direct children of the given cursor, invoking
      the given visitor function with the cursors of each visited child. The
      traversal may be recursive, if the visitor returns
      TChildVisitResult.Recurse. The traversal may also be ended prematurely, if
      the visitor returns TChildVisitResult.Break. }
    function VisitChildren(const AVisitor: TCursorVisitor): Boolean; overload;
    function VisitChildren(const AVisitor: TCursorVisitorMethod): Boolean; overload;

    { Find references of a declaration in a specific file.

      Parameters:
        AFile: file to search for references.
        AVisitor: callback that will receive pairs of TCursor/TSourceRange for
          each reference found. The TSourceRange will point inside the file; if
          the reference is inside a macro (and not a macro argument) the
          TSourceRange will be invalid.

      Returns:
        A TVisitResult value }
    function FindReferencesInFile(const AFile: TFile;
      const AVisitor: TCursorAndRangeVisitor): TVisitorResult; overload;
    function FindReferencesInFile(const AFile: TFile;
      const AVisitor: TCursorAndRangeVisitorMethod): TVisitorResult; overload;
  end;

  { Visitor invoked for each field found by a traversal.

    This visitor will be invoked for each field found by TType.VisitFields. Its
    argument is the cursor being visited.

    The visitor should return one of the TVisitorResult values to direct
    TType.VisitFields. }
  TFieldVisitor = reference to function(const ACursor: TCursor): TVisitorResult;
  TFieldVisitorMethod = function(const ACursor: TCursor): TVisitorResult of object;

  { Extends TType }
  TTypeHelper = record helper for TType
  {$REGION 'Internal Declarations'}
  private
    function GetDeclaration: TCursor; inline;
  private
    class function FieldVisitor(ACursor: TCXCursor;
      AClientData: TCXClientData): TCXVisitorResult; cdecl; static;
  {$ENDREGION 'Internal Declarations'}
  public
    { Visit the fields of this type.

      Parameters:
        AVisitor: the visitor function that will be invoked for each field.

      Returns:
        True if the traversal was terminated prematurely by the visitor
        returning TVisitorResult.Break.

      This function visits all the direct fields by invoking the given AVisitor
      with the cursors of each visited field. The traversal may be ended
      prematurely, if the visitor returns TVisitorResult.Break. }
    function VisitFields(const AVisitor: TFieldVisitor): Boolean; overload;
    function VisitFields(const AVisitor: TFieldVisitorMethod): Boolean; overload;

    { The cursor for the declaration of the given type. }
    property Declaration: TCursor read GetDeclaration;
  end;

type
  { A fast container representing a set of cursors.
    Implemented by TCursorSet. }
  ICursorSet = interface
  ['{91598024-E848-45E5-B1C9-DD0EB27E7610}']
    {$REGION 'Internal Declarations'}
    function GetHandle: TCXCursorSet;
    {$ENDREGION 'Internal Declarations'}

    { Queries the cursor set to see if it contains a specific cursor. }
    function Contains(const ACursor: TCursor): Boolean;

    { Inserts a cursor into the cursor set.

      Returns:
        False if the cursor was already in the set. }
    function Insert(const ACursor: TCursor): Boolean;

    { Internal handle to C API }
    property Handle: TCXCursorSet read GetHandle;
  end;

type
  { Describes a single preprocessing token. }
  TToken = record
  {$REGION 'Internal Declarations'}
  private
    FHandle: TCXToken;
    function GetKind: TTokenKind; inline;
  {$ENDREGION 'Internal Declarations'}
  public
    { The kind of the given token. }
    property Kind: TTokenKind read GetKind;

    { Internal handle to C API }
    property Handle: TCXToken read FHandle;
  end;

type
  { A list of tokens }
  ITokenList = interface
  ['{3C67C0CF-052B-4F60-9F64-58311B72BFEC}']
    {$REGION 'Internal Declarations'}
    function GetCount: Integer;
    function GetToken(const AIndex: Integer): TToken;
    {$ENDREGION 'Internal Declarations'}

    { Number of tokens in the list }
    property Count: Integer read GetCount;

    { The tokens in the list }
    property Tokens[const AIndex: Integer]: TToken read GetToken; default;
  end;

type
  { A result of code-completion. }
  TCompletionResult = record
  {$REGION 'Internal Declarations'}
  private
    FHandle: TCXCompletionResult;
    function GetKind: TCursorKind; inline;
    function GetCompletionString: TCompletionString; inline;
  {$ENDREGION 'Internal Declarations'}
  public
    { Sort an array of code-completion results in case-insensitive alphabetical
      order. }
    class procedure Sort(var AResults: TArray<TCompletionResult>); static;

    { The kind of entity that this completion refers to.

      The cursor kind will be a macro, keyword, or a declaration (one of
      the *Decl cursor kinds), describing the entity that the completion is
      referring to. }
    property Kind: TCursorKind read GetKind;

    { The code-completion string that describes how to insert this
      code-completion result into the editing buffer. }
    property CompletionString: TCompletionString read GetCompletionString;

    { Internal handle to C API }
    property Handle: TCXCompletionResult read FHandle;
  end;

type
  IDiagnosticSet = interface;

  { A single diagnostic, containing the diagnostic's severity, location, text,
    source ranges, and fix-it hints. }
  IDiagnostic = interface
  ['{41A68BA7-543A-49DB-A7E0-636798B2D923}']
    {$REGION 'Internal Declarations'}
    function GetChildDiagnostics: IDiagnosticSet;
    function GetSeverity: TDiagnosticSeverity;
    function GetLocation: TSourceLocation;
    function GetSpelling: String;
    function GetEnableOption: String;
    function GetDisableOption: String;
    function GetCategory: Integer;
    function GetCategoryText: String;
    function GetRangeCount: Integer;
    function GetRange(const AIndex: Integer): TSourceRange;
    function GetFixItCount: Integer;
    function GetHandle: TCXDiagnostic;
    {$ENDREGION 'Internal Declarations'}

    { Format the diagnostic in a manner that is suitable for display.

      Parameters:
        AOptions: a set of options that control the diagnostic display.

      Returns:
        The formatted diagnostic.

      This routine will format the diagnostic to a string, renderingthe
      diagnostic according to the various options given. The
      GetDefaultDiagnosticDisplayOptions function returns the set of options
      that most closely mimics the behavior of the clang compiler. }
    function Format(const AOptions: TDiagnosticDisplayOptions): String;

    { Retrieve the replacement information for a given fix-it.

      Parameters:
        AFixItIndex: the zero-based index of the fix-it (0..FixItCount-1).
        AReplacementRange: is set to the source range whose contents should be
          replaced with the returned replacement string. Note that source ranges
          are half-open ranges [a, b@), so the source code should be replaced
          from a and up to (but not including) b.

      Returns:
        A string containing text that should be replace the source code
        indicated by AReplacementRange.

      Fix-its are described in terms of a source range whose contents should be
      replaced by a string. This approach generalizes over three kinds of
      operations: removal of source code (the range covers the code to be
      removed and the replacement string is empty), replacement of source code
      (the range covers the code to be replaced and the replacement string
      provides the new code), and insertion (both the start and end of the range
      point at the insertion location, and the replacement string provides the
      text to insert). }
    function FixIt(const AFixItIndex: Integer;
      out AReplacementRange: TSourceRange): String;

    { Child diagnostics }
    property ChildDiagnostics: IDiagnosticSet read GetChildDiagnostics;

    { The severity of the diagnostic. }
    property Severity: TDiagnosticSeverity read GetSeverity;

    { The source location of the diagnostic.

      This location is where Clang would print the caret ('^') when displaying
      the diagnostic on the command line. }
    property Location: TSourceLocation read GetLocation;

    { The text of the diagnostic. }
    property Spelling: String read GetSpelling;

    { The name of the command-line option that enabled this diagnostic, such as
      "-Wconversion" or "-pedantic". }
    property EnableOption: String read GetEnableOption;

    { The name of the command-line option that disables this diagnostic. }
    property DisableOption: String read GetDisableOption;

    { The category number for this diagnostic.

      Diagnostics can be categorized into groups along with other, related
      diagnostics (e.g., diagnostics under the same warning flag). This property
      retrieves the category number for the diagnostic, or zero if this
      diagnostic is uncategorized.}
    property Category: Integer read GetCategory;

    { The diagnostic category text for the diagnostic. }
    property CategoryText: String read GetCategoryText;

    { The number of source ranges associated with the given diagnostic. }
    property RangeCount: Integer read GetRangeCount;

    { Source ranges associated with the diagnostic.

      A diagnostic's source ranges highlight important elements in the source
      code. On the command line, Clang displays source ranges by underlining
      them with '~' characters. }
    property Ranges[const AIndex: Integer]: TSourceRange read GetRange;

    { The number of fix-it hints associated with the given diagnostic. }
    property FixItCount: Integer read GetFixItCount;

    { Internal handle to C API }
    property Handle: TCXDiagnostic read GetHandle;
  end;

  { A group of IDiagnostics. }
  IDiagnosticSet = interface
  ['{967B9561-6222-4CFB-989E-CCD2450EB9E9}']
    {$REGION 'Internal Declarations'}
    function GetCount: Integer;
    function GetDiagnostic(const AIndex: Integer): IDiagnostic;
    function GetHandle: TCXDiagnosticSet;
    {$ENDREGION 'Internal Declarations'}

    { Number of diagnostics }
    property Count: Integer read GetCount;

    { The diagnostics }
    property Diagnostics[const AIndex: Integer]: IDiagnostic read GetDiagnostic; default;

    { Internal handle to C API }
    property Handle: TCXDiagnosticSet read GetHandle;
  end;

type
  { Contains the results of code-completion. }
  ICodeCompleteResults = interface
  ['{0CCB125C-8793-4038-9F96-8063BFC6FEDD}']
    {$REGION 'Internal Declarations'}
    function GetCount: Integer;
    function GetResult(const AIndex: Integer): TCompletionResult;
    function GetDiagnosticCount: Integer;
    function GetDiagnostic(const AIndex: Integer): IDiagnostic;
    function GetContexts: UInt64;
    function GetContainerKind: TCursorKind;
    function GetContainerComplete: Boolean;
    function GetContainerUsr: TUnifiedSymbolResolution;
    function GetObjCSelector: String;
    function GetFixItCount(const AIndex: Integer): Integer;
    function GetHandle: PCXCodeCompleteResults;
    {$ENDREGION 'Internal Declarations'}

    { Fix-its that *must* be applied before inserting the text for the
      corresponding completion.

      Parameters:
        AIndex: The index of the completion.
        AFixItIndex: The index of the fix-it for the completion at AIndex
          (see FixItCount).
        AReplacementRange: The fix-it range that must be replaced before the
          completion at AIndex can be applied.

      Returns:
        The fix-it string that must replace the code at AReplacementRange
        before the completion at AIndex can be applied.

      By default, TTranslationUnit.CodeCompleteAt only returns completions with
      empty fix-its. Extra completions with non-empty fix-its should be
      explicitly requested by setting
      TCodeCompleteFlag.IncludeCompletionsWithFixIts.

      For the clients to be able to compute position of the cursor after
      applying fix-its, the following conditions are guaranteed to hold for
      replacement_range of the stored fix-its:
      * Ranges in the fix-its are guaranteed to never contain the completion
        point (or identifier under completion point, if any) inside them, except
        at the start or at the end of the range.
      * If a fix-it range starts or ends with completion point (or starts or
        ends after the identifier under completion point), it will contain at
        least one character. It allows to unambiguously recompute completion
        point after applying the fix-it.

      The intuition is that provided fix-its change code around the identifier
      we complete, but are not allowed to touch the identifier itself or the
      completion point. One example of completions with corrections are the ones
      replacing '.' with '->' and vice versa:

        std::unique_ptr<std::vector<int>> vec_ptr;

      In 'vec_ptr.^', one of the completions is 'push_back', it requires
      replacing '.' with '->'.

      In 'vec_ptr->^', one of the completions is 'release', it requires
      replacing '->' with '.'. }
    function GetCompletionFixIt(const AIndex, AFixItIndex: Integer;
      const AReplacementRange: TSourceRange): String;

    { Number of code completion results }
    property Count: Integer read GetCount;

    { Code completion results }
    property Results[const AIndex: Integer]: TCompletionResult read GetResult;

    { The number of diagnostics produced prior to the location where code
      completion was performed. }
    property DiagnosticCount: Integer read GetDiagnosticCount;

    { The diagnostics produced prior to the location where code completion was
      performed. }
    property Diagnostics[const AIndex: Integer]: IDiagnostic read GetDiagnostic;

    { The kinds of completions that are appropriate for use along with the code
      completion results. }
    property Contexts: UInt64 read GetContexts;

    { The cursor kind for the container for the current code completion context.
      The container is only guaranteed to be set for contexts where a container
      exists (i.e. member accesses or Objective-C message sends); if there is
      not a container, this function will return InvalidCode. }
    property ContainerKind: TCursorKind read GetContainerKind;

    { Whether Clang has complete information about the container for the current
      code completion context. }
    property ContainerComplete: Boolean read GetContainerComplete;

    { The USR for the container for the current code completion context. If
      there is not a container for the current context, this function will
      return the empty string. }
    property ContainerUsr: TUnifiedSymbolResolution read GetContainerUsr;

    { The currently-entered selector for an Objective-C message send, formatted
      like "initWithFoo:bar:". Only guaranteed to return a non-empty string for
      ObjCInstanceMessage and ObjCClassMessage. }
    property ObjCSelector: String read GetObjCSelector;

    { The number of fix-its for the given completion index.

      Calling this makes sense only if
      TCodeCompleteFlag.IncludeCompletionsWithFixIts option was set.

      Parameters:
        AIndex: The index of the completion

      Returns:
        The number of fix-its which must be applied before the completion at
        completion_index can be applied }
    property FixItCount[const AIndex: Integer]: Integer read GetFixItCount;

    { Internal handle to C API }
    property Handle: PCXCodeCompleteResults read GetHandle;
  end;

type
  { Visitor invoked for each file in a translation unit (used with
    ITranslationUnit.GetInclusions).

    This visitor (anonymous) method will be invoked by
    ITranslationUnit.GetInclusions for each file included (either at the
    top-level or by #include directives) within a translation unit. The first
    argument is the file being included, and the second and third arguments
    provide the inclusion stack. The array is sorted in order of immediate
    inclusion. For example, the first element refers to the location that
    included 'included_file'. }
  TInclusionVisitor = reference to procedure(const AIncludedFile: TFile;
    const AInclusionStack: PSourceLocation; const AInclusionStackCount: Integer);
  TInclusionVisitorMethod = procedure(const AIncludedFile: TFile;
    const AInclusionStack: PSourceLocation; const AInclusionStackCount: Integer) of object;

type
  { Entry in the IResourceUsage list }
  TResourceUsageEntry = record
  {$REGION 'Internal Declarations'}
  private
    FHandle: TCXTUResourceUsageEntry;
    function GetKind: TResourceUsageKind; inline;
    function GetName: String; inline;
    function GetAmount: Integer; inline;
  {$ENDREGION 'Internal Declarations'}
  public
    { The memory usage category }
    property Kind: TResourceUsageKind read GetKind;

    { Human-readable string that represents the name of the memory category. }
    property Name: String read GetName;

    { Amount of resources used. The units will depend on the resource kind. }
    property Amount: Integer read GetAmount;

    { Internal handle to C API }
    property Handle: TCXTUResourceUsageEntry read FHandle;
  end;

type
  { The memory usage of an ITranslationUnit, broken into categories. }
  IResourceUsage = interface
  ['{DCFEACEF-8A83-484B-AC5C-DFAC56E59D55}']
    {$REGION 'Internal Declarations'}
    function GetHandle: TCXTUResourceUsage;
    {$ENDREGION 'Internal Declarations'}

    { Internal handle to C API }
    property Handle: TCXTUResourceUsage read GetHandle;
  end;

type
  { Target information for a translation unit. }
  ITargetInfo = interface
  ['{3588F66D-5D4E-4128-8438-77D649BF550E}']
    {$REGION 'Internal Declarations'}
    function GetTriple: String;
    function GetPointerWidth: Integer;
    function GetHandle: TCXTargetInfo;
    {$ENDREGION 'Internal Declarations'}

    { The normalized target triple as a string.
      Returns the empty string in case of any error. }
    property Triple: String read GetTriple;

    { The pointer width of the target in bits.
      Returns -1 in case of error. }
    property PointerWidth: Integer read GetPointerWidth;

    { Internal handle to C API }
    property Handle: TCXTargetInfo read GetHandle;
  end;

type
  { A single translation unit, which resides in an IIndex. }
  ITranslationUnit = interface
  ['{FC9A4ABF-B285-47AF-A063-C8C85CABE02E}']
    {$REGION 'Internal Declarations'}
    function GetDiagnosticCount: Integer;
    function GetDiagnostic(const AIndex: Integer): IDiagnostic;
    function GetAllDiagnostics: IDiagnosticSet;
    function GetSpelling: String;
    function GetResourceUsage: IResourceUsage;
    function GetTargetInfo: ITargetInfo;
    function GetCursor: TCursor; overload;
    function GetHandle: TCXTranslationUnit;
    {$ENDREGION 'Internal Declarations'}

    { Retrieve a file handle within this translation unit.

      Parameters:
        AFilename: the name of the file.

      Returns:
        The file handle for the named file in the translation unit, or a Null
        file handle if the file was not a part of this translation unit. }
    function GetFile(const AFilename: String): TFile;

    { Retrieve the buffer associated with the given file.

      Parameters:
        AFile: the file for which to retrieve the buffer.

      Returns:
        The contents of the file, or nil when the file is not loaded. }
    function GetFileContents(const AFile: TFile): TBytes;

    { Determine whether the given header is guarded against multiple inclusions,
      either with the conventional #ifndef, #define, #endif macro guards or with
      #pragma once. }
    function IsFileMultipleIncludeGuarded(const AFile: TFile): Boolean;

    { Retrieves the source location associated with a given file/line/column
      in a particular translation unit. }
    function GetLocation(const AFile: TFile; const ALine,
      AColumn: Integer): TSourceLocation; overload;

    { Retrieves the source location associated with a given character offset
      in a particular translation unit.}
    function GetLocation(const AFile: TFile;
      const AOffset: Integer): TSourceLocation; overload;

    { Retrieve all ranges that were skipped by the preprocessor.

      The preprocessor will skip lines when they are surrounded by an
      if/ifdef/ifndef directive whose condition does not evaluate to True. }
    function GetSkippedRanges(const AFile: TFile): ISourceRangeList;

    { Retrieve all ranges from all files that were skipped by the preprocessor.

      The preprocessor will skip lines when they are surrounded by an
      if/ifdef/ifndef directive whose condition does not evaluate to True. }
    function GetAllSkippedRanges: ISourceRangeList;

    { Saves the translation unit into a serialized representation of that
      translation unit on disk.

      Parameters:
        AFilename: The file to which the translation unit will be saved.

      Returns:
        Save result.

      Any translation unit that was parsed without error can be saved into a
      file. The translation unit can then be deserialized into a new
      ITranslationUnit with IIndex.CreateTranslationUnit or, if it is an
      incomplete translation unit that corresponds to a header, used as a
      precompiled header when parsing other translation units. }
    function Save(const AFilename: String): TSaveResult;

    { Suspend a translation unit in order to free memory associated with it.

      A suspended translation unit uses significantly less memory but on the
      other side does not support any other calls than Reparse to resume it. }
    procedure Suspend;

    { Reparse the source files that produced this translation unit.

      Parameters:
        AUnsavedFiles: (optional) array of files that have not yet been saved to
          disk but may be required for code completion, including the contents
          of those files.

      Returns:
        AN error code.

      This routine can be used to re-parse the source files that originally
      created the given translation unit, for example because those source files
      have changed (either on disk or as passed via AUnsavedFiles). The source
      code will be reparsed with the same command-line options as it was
      originally parsed.

      Reparsing a translation unit invalidates all cursors and source locations
      that refer into that translation unit. This makes reparsing a translation
      unit semantically equivalent to destroying the translation unit and then
      creating a new translation unit with the same command-line arguments.
      However, it may be more efficient to reparse a translation unit using this
      routine.

      NOTE: The translation unit must originally have been built with
      IIndex.CreateTranslationUnitFromSource. }
    function Reparse(const AUnsavedFiles: TArray<TUnsavedFile> = nil): TError;

    { Map a source location to the cursor that describes the entity at that
      location in the source code.

      Returns:
        A cursor representing the entity at the given source location, or
        a NULL cursor if no such entity can be found.

      Maps an arbitrary source location within the translation unit down to the
      most specific cursor that describes the entity at that location. For
      example, given an expression "x + y", invoking GetCursor with a source
      location pointing to "x" will return the cursor for "x"; similarly for
      "y". If the cursor points anywhere between "x" or "y" (e.g., on the + or
      the whitespace around it), clang_getCursor() will return a cursor
      referring to the "+" expression. }
    function GetCursor(const ASourceLocation: TSourceLocation): TCursor; overload;

    { Given a TFile header file, return the module that contains it, if one
      exists. }
    function GetModule(const AFile: TFile): TModule;

    { The number of top level headers associated with a module. }
    function GetTopLevelHeaderCount(const AModule: TModule): Integer;

    { The top level headers associated with a module. }
    function GetTopLevelHeader(const AModule: TModule; const AIndex: Integer): TFile;

    { Tokenize the source code described by the given range into raw lexical
      tokens.

      Parameters:
        ARange: the source range in which text should be tokenized. All of the
          tokens produced by tokenization will fall within this source range,

      Returns:
        The list of tokens. }
    function Tokenize(const ARange: TSourceRange): ITokenList;

    { Annotate the given set of tokens by providing cursors for each token
      that can be mapped to a specific entity within the abstract syntax tree.

      Parameters:
        ATokens: the set of tokens to annotate.

      Returns:
        The cursors corresponding to each token.

      This token-annotation routine is equivalent to invoking GetCursor for the
      source locations of each of the tokens. The cursors provided are filtered,
      so that only those cursors that have a direct correspondence to the token
      are accepted. For example, given a function call "f(x)", GetCursor would
      provide the following cursors:
      * when the cursor is over the 'f', a DeclRefExpr cursor referring to 'f'.
      * when the cursor is over the '(' or the ')', a CallExpr referring to 'f'.
      * when the cursor is over the 'x', a DeclRefExpr cursor referring to 'x'.

      Only the first and last of these cursors will occur within the annotate,
      since the tokens "f" and "x" directly refer to a function and a variable,
      respectively, but the parentheses are just a small part of the full syntax
      of the function call expression, which is not provided as an annotation. }
    function AnnotateTokens(const ATokens: ITokenList): TArray<TCursor>; overload;
    function AnnotateTokens(const ATokens: TArray<TToken>): TArray<TCursor>; overload;

    { Determine the spelling of the given token.

      The spelling of a token is the textual representation of that token, e.g.,
      the text of an identifier or keyword. }
    function GetTokenSpelling(const AToken: TToken): String;

    { Retrieve the source location of the given token. }
    function GetTokenLocation(const AToken: TToken): TSourceLocation;

    { Retrieve a source range that covers the given token. }
    function GetTokenExtent(const AToken: TToken): TSourceRange;

    { Perform code completion at a given location in the translation unit.

      Parameters:
        ACompleteFilename: The name of the source file where code completion
          should be performed. This filename may be any file included in the
          translation unit.
        ACompleteLine: The line at which code-completion should occur.
        ACompleteColumn: The column at which code-completion should occur.
          Note that the column should point just after the syntactic construct
          that initiated code completion, and not in the middle of a lexical
          token.
        AUnsavedFiles: (optional) array of files that have not yet been saved to
          disk but may be required for parsing or code completion, including the
          contents of those files.
        AOptions: (optional) extra options that control the behavior of code
          completion. The GetDefaultCodeCompleteOptions function returns a
          default set of code-completion options.

      Returns:
        If successful, a new ICodeCompleteResults object containing
        code-completion results. If code completion fails, returns nil.

      This function performs code completion at a particular file, line, and
      column within source code, providing results that suggest potential
      code snippets based on the context of the completion. The basic model
      for code completion is that Clang will parse a complete source file,
      performing syntax checking up to the location where code-completion has
      been requested. At that point, a special code-completion token is passed
      to the parser, which recognizes this token and determines, based on the
      current location in the C/Objective-C/C++ grammar and the state of
      semantic analysis, what completions to provide. These completions are
      returned via a new ICodeCompleteResults object.

      Code completion itself is meant to be triggered by the client when the
      user types punctuation characters or whitespace, at which point the
      code-completion location will coincide with the cursor. For example, if
      "p" is a pointer, code-completion might be triggered after the "-" and
      then after the ">" in "p->". When the code-completion location is afer the
      ">", the completion results will provide, e.g., the members of the struct
      that "p" points to. The client is responsible for placing the cursor at
      the beginning of the token currently being typed, then filtering the
      results based on the contents of the token. For example, when
      code-completing for the expression "p->get", the client should provide the
      location just after the ">" (e.g., pointing at the "g") to this
      code-completion hook. Then, the client can filter the results based on the
      current token text ("get"), only showing those results that start with
      "get". The intent of this interface is to separate the relatively
      high-latency acquisition of code-completion results from the filtering of
      results on a per-character basis, which must have a lower latency.

      NOTE: The source files for this translation unit need not be completely
      up-to-date (and the contents of those source files may be overridden via
      AUnsavedFiles). Cursors referring into the translation unit may be
      invalidated by this invocation. }
    function CodeCompleteAt(const ACompleteFilename: String; const ACompleteLine,
      ACompleteColumn: Integer; const AUnsavedFiles: TArray<TUnsavedFile> = nil;
      const AOptions: TCodeCompleteFlags = []): ICodeCompleteResults;

    { Visit the set of preprocessor inclusions in a translation unit.
      The visitor (anonymous) method is called with the provided data for every
      included file. This does not include headers included by the PCH file
      (unless one is inspecting the inclusions in the PCH file itself). }
    procedure GetInclusions(const AVisitor: TInclusionVisitor); overload;
    procedure GetInclusions(const AVisitor: TInclusionVisitorMethod); overload;

    { Find #import/#include directives in a specific file.

      Parameters:
        AFile: file to search for #import/#include directives.
        AVisitor: visitor callback that will receive pairs of
          TCursor/TSourceRange for each directive found.

      Returns:
        A visit result }
    function FindIncludesInFile(const AFile: TFile;
      const AVisitor: TCursorAndRangeVisitor): TVisitResult; overload;
    function FindIncludesInFile(const AFile: TFile;
      const AVisitor: TCursorAndRangeVisitorMethod): TVisitResult; overload;

    { Get the raw lexical token starting with the given location.

      Parameters:
        ALocation: the source location with which the token starts.

      Returns:
        A list containing the single token starting with the given location or
        nil if no such token  exist. }
    function GetToken(const ALocation: TSourceLocation): ITokenList;

    { The number of diagnostics produced for this translation unit. }
    property DiagnosticCount: Integer read GetDiagnosticCount;

    { The diagnostics produced for this translation unit. }
    property Diagnostics[const AIndex: Integer]: IDiagnostic read GetDiagnostic;

    { The complete set of diagnostics associated with a translation unit. }
    property AllDiagnostics: IDiagnosticSet read GetAllDiagnostics;

    { Get the original translation unit source file name. }
    property Spelling: String read GetSpelling;

    { The memory usage of this translation unit. }
    property ResourceUsage: IResourceUsage read GetResourceUsage;

    { Target information for this translation unit. }
    property TargetInfo: ITargetInfo read GetTargetInfo;

    { The cursor that represents this translation unit.

      The translation unit cursor can be used to start traversing the various
      declarations within the given translation unit. }
    property Cursor: TCursor read GetCursor;

    { Internal handle to C API }
    property Handle: TCXTranslationUnit read GetHandle;
  end;

type
  { The client's data object that is associated with a TFile.}
  TIdxClientFile = Pointer;

type
  { The client's data object that is associated with a semantic container
    of entities.}
  TIdxClientContainer = Pointer;

type
  { The client's data object that is associated with a semantic entity. }
  TIdxClientEntity = Pointer;

type
  { Source location passed to IIndexerListener callbacks. }
  TIdxLoc = record
  {$REGION 'Internal Declarations'}
  private
    FHandle: TCXIdxLoc;
    function GetSourceLocation: TSourceLocation; inline;
  {$ENDREGION 'Internal Declarations'}
  public
    { Retrieve the TIdxFile, file, line, column, and offset represented by
      this TIdxLoc.

      If the location refers into a macro expansion, retrieves the location of
      the macro expansion and if it refers into a macro argument retrieves the
      location of the argument. }
    procedure GetFileLocation(out AIndexFile: TIdxClientFile; out AFile: TFile;
      out ALine, AColumn, AOffset: Integer);

    { The source location represented by this TIdxLoc.}
    property SourceLocation: TSourceLocation read GetSourceLocation;

    { Internal handle to C API }
    property Handle: TCXIdxLoc read FHandle;
  end;

type
  { Data for the IIndexerListener.IncludedFile callback }
  TIdxIncludedFileInfo = record
  {$REGION 'Internal Declarations'}
  private
    FHandle: PCXIdxIncludedFileInfo;
    function GetHashLoc: TIdxLoc; inline;
    function GetFilename: String; inline;
    function GetIncludedFile: TFile; inline;
    function GetIsImport: Boolean; inline;
    function GetIsAngled: Boolean; inline;
    function GetIsModuleImport: Boolean; inline;
  {$ENDREGION 'Internal Declarations'}
  public
    { Location of '#' in the #include/#import directive. }
    property HashLoc: TIdxLoc read GetHashLoc;

    { Filename as written in the #include/#import directive. }
    property Filename: String read GetFilename;

    { The actual file that the #include/#import directive resolved to. }
    property IncludedFile: TFile read GetIncludedFile;

    property IsImport: Boolean read GetIsImport;
    property IsAngled: Boolean read GetIsAngled;

    { True if the directive was automatically turned into a module import.}
    property IsModuleImport: Boolean read GetIsModuleImport;

    { Internal handle to C API }
    property Handle: PCXIdxIncludedFileInfo read FHandle;
  end;

type
  { Data for IIndexerListener.ImportedAstFile. }
  TIdxImportedAstFileInfo = record
  {$REGION 'Internal Declarations'}
  private
    FHandle: PCXIdxImportedASTFileInfo;
    function GetAstFile: TFile; inline;
    function GetModule: TModule; inline;
    function GetLoc: TIdxLoc; inline;
    function GetIsImplicit: Boolean; inline;
  {$ENDREGION 'Internal Declarations'}
  public
    { Top level AST file containing the imported PCH, module or submodule. }
    property AstFile: TFile read GetAstFile;

    { The imported module or Null if the AST file is a PCH. }
    property Module: TModule read GetModule;

    { Location where the file is imported. Applicable only for modules. }
    property Loc: TIdxLoc read GetLoc;

    { True if an inclusion directive was automatically turned into a module
      import. Applicable only for modules. }
    property IsImplicit: Boolean read GetIsImplicit;

    { Internal handle to C API }
    property Handle: PCXIdxImportedASTFileInfo read FHandle;
  end;

type
  TIdxEntityKind = (
    Unexposed = CXIdxEntity_Unexposed,
    Typedef = CXIdxEntity_Typedef,
    Func = CXIdxEntity_Function,
    Variable = CXIdxEntity_Variable,
    Field = CXIdxEntity_Field,
    EnumConstant = CXIdxEntity_EnumConstant,
    ObjCClass = CXIdxEntity_ObjCClass,
    ObjCProtocol = CXIdxEntity_ObjCProtocol,
    ObjCCategory = CXIdxEntity_ObjCCategory,
    ObjCInstanceMethod = CXIdxEntity_ObjCInstanceMethod,
    ObjCClassMethod = CXIdxEntity_ObjCClassMethod,
    ObjCProperty = CXIdxEntity_ObjCProperty,
    ObjCIvar = CXIdxEntity_ObjCIvar,
    Enum = CXIdxEntity_Enum,
    Struct = CXIdxEntity_Struct,
    Union = CXIdxEntity_Union,
    CXXClass = CXIdxEntity_CXXClass,
    CXXNamespace = CXIdxEntity_CXXNamespace,
    CXXNamespaceAlias = CXIdxEntity_CXXNamespaceAlias,
    CXXStaticVariable = CXIdxEntity_CXXStaticVariable,
    CXXStaticMethod = CXIdxEntity_CXXStaticMethod,
    CXXInstanceMethod = CXIdxEntity_CXXInstanceMethod,
    CXXConstructor = CXIdxEntity_CXXConstructor,
    CXXDestructor = CXIdxEntity_CXXDestructor,
    CXXConversionFunction = CXIdxEntity_CXXConversionFunction,
    CXXTypeAlias = CXIdxEntity_CXXTypeAlias,
    CXXInterface = CXIdxEntity_CXXInterface);

type
  TIdxEntityCxxTemplateKind = (
    NonTemplate = CXIdxEntity_NonTemplate,
    Template = CXIdxEntity_Template,
    TemplatePartialSpecialization = CXIdxEntity_TemplatePartialSpecialization,
    TemplateSpecialization = CXIdxEntity_TemplateSpecialization);

type
  TIdxEntityLanguage = (
    None = CXIdxEntityLang_None,
    C = CXIdxEntityLang_C,
    ObjC = CXIdxEntityLang_ObjC,
    CXX = CXIdxEntityLang_CXX,
    Swift = CXIdxEntityLang_Swift);

type
  TIdxAttrKind = (
    Unexposed = CXIdxAttr_Unexposed,
    IBAction = CXIdxAttr_IBAction,
    IBOutlet = CXIdxAttr_IBOutlet,
    IBOutletCollection = CXIdxAttr_IBOutletCollection);

type
  { Information about an indexed attribute }
  TIdxAttrInfo = record
  {$REGION 'Internal Declarations'}
  private
    FHandle: PCXIdxAttrInfo;
    function GetKind: TIdxAttrKind; inline;
    function GetCursor: TCursor; inline;
    function GetLoc: TIdxLoc; inline;
  {$ENDREGION 'Internal Declarations'}
  public
    property Kind: TIdxAttrKind read GetKind;
    property Cursor: TCursor read GetCursor;
    property Loc: TIdxLoc read GetLoc;

    { Internal handle to C API }
    property Handle: PCXIdxAttrInfo read FHandle;
  end;

type
  { Data for TIdxDeclInfo.EntityInfo. }
  TIdxEntityInfo = record
  {$REGION 'Internal Declarations'}
  private
    FHandle: PCXIdxEntityInfo;
    function GetKind: TIdxEntityKind; inline;
    function GetIsObjCContainer: Boolean; inline;
    function GetTemplateKind: TIdxEntityCxxTemplateKind; inline;
    function GetLang: TIdxEntityLanguage; inline;
    function GetName: String; inline;
    function GetUsr: String; inline;
    function GetCursor: TCursor; inline;
    function GetAttributes: TArray<TIdxAttrInfo>;
    function GetClientEntity: TIdxClientEntity; inline;
    procedure SetClientEntity(const AValue: TIdxClientEntity); inline;
  {$ENDREGION 'Internal Declarations'}
  public
    property Kind: TIdxEntityKind read GetKind;
    property IsObjCContainer: Boolean read GetIsObjCContainer;
    property TemplateKind: TIdxEntityCxxTemplateKind read GetTemplateKind;
    property Lang: TIdxEntityLanguage read GetLang;
    property Name: String read GetName;
    property Usr: String read GetUsr;
    property Cursor: TCursor read GetCursor;
    property Attributes: TArray<TIdxAttrInfo> read GetAttributes;

    { A custom TIdxClientEntity attached to the entity. }
    property ClientEntity: TIdxClientEntity read GetClientEntity write SetClientEntity;

    { Internal handle to C API }
    property Handle: PCXIdxEntityInfo read FHandle;
  end;

type
  { Information about an indexed container }
  TIdxContainerInfo = record
  {$REGION 'Internal Declarations'}
  private
    FHandle: PCXIdxContainerInfo;
    function GetCursor: TCursor; inline;
    function GetClientContainer: TIdxClientContainer; inline;
    procedure SetClientContainer(const AValue: TIdxClientContainer); inline;
  {$ENDREGION 'Internal Declarations'}
  public
    property Cursor: TCursor read GetCursor;

    { A custom TIdxClientContainer attached to the container.}
    property ClientContainer: TIdxClientContainer read GetClientContainer write SetClientContainer;

    { Internal handle to C API }
    property Handle: PCXIdxContainerInfo read FHandle;
  end;

type
  { Data for IIndexerListener.IndexDeclaration. }
  TIdxDeclInfo = record
  {$REGION 'Internal Declarations'}
  private
    FHandle: PCXIdxDeclInfo;
    function GetEntityInfo: TIdxEntityInfo; inline;
    function GetCursor: TCursor; inline;
    function GetLoc: TIdxLoc; inline;
    function GetSemanticContainer: TIdxContainerInfo; inline;
    function GetLexicalContainer: TIdxContainerInfo; inline;
    function GetIsRedeclaration: Boolean; inline;
    function GetIsDefinition: Boolean; inline;
    function GetIsContainer: Boolean; inline;
    function GetDeclAsContainer: TIdxContainerInfo; inline;
    function GetIsImplicit: Boolean; inline;
    function GetAttributes: TArray<TIdxAttrInfo>;
    function GetFlags: Cardinal; inline;
  {$ENDREGION 'Internal Declarations'}
  public
    property EntityInfo: TIdxEntityInfo read GetEntityInfo;
    property Cursor: TCursor read GetCursor;
    property Loc: TIdxLoc read GetLoc;
    property SemanticContainer: TIdxContainerInfo read GetSemanticContainer;

    { Generally same as SemanticContainer but can be different in cases like
      out-of-line C++ member functions.}
    property LexicalContainer: TIdxContainerInfo read GetLexicalContainer;

    property IsRedeclaration: Boolean read GetIsRedeclaration;
    property IsDefinition: Boolean read GetIsDefinition;
    property IsContainer: Boolean read GetIsContainer;
    property DeclAsContainer: TIdxContainerInfo read GetDeclAsContainer;

    { Whether the declaration exists in code or was created implicitly by the
      compiler, e.g. implicit Objective-C methods for properties. }
    property IsImplicit: Boolean read GetIsImplicit;

    property Attributes: TArray<TIdxAttrInfo> read GetAttributes;
    property Flags: Cardinal read GetFlags;

    { Internal handle to C API }
    property Handle: PCXIdxDeclInfo read FHandle;
  end;

type
  TIdxObjCContainerKind = (
    ForwardRef = CXIdxObjCContainer_ForwardRef,
    Interf = CXIdxObjCContainer_Interface,
    Impl = CXIdxObjCContainer_Implementation);

type
  TIdxObjCContainerDeclInfo = record
  {$REGION 'Internal Declarations'}
  private
    FHandle: PCXIdxObjCContainerDeclInfo;
    function GetDeclInfo: TIdxDeclInfo; inline;
    function GetKind: TIdxObjCContainerKind; inline;
  {$ENDREGION 'Internal Declarations'}
  public
    property DeclInfo: TIdxDeclInfo read GetDeclInfo;
    property Kind: TIdxObjCContainerKind read GetKind;

    { Internal handle to C API }
    property Handle: PCXIdxObjCContainerDeclInfo read FHandle;
  end;

type
  TIdxBaseClassInfo = record
  {$REGION 'Internal Declarations'}
  private
    FHandle: PCXIdxBaseClassInfo;
    function GetBase: TIdxEntityInfo; inline;
    function GetCursor: TCursor; inline;
    function GetLoc: TIdxLoc; inline;
  {$ENDREGION 'Internal Declarations'}
  public
    property Base: TIdxEntityInfo read GetBase;
    property Cursor: TCursor read GetCursor;
    property Loc: TIdxLoc read GetLoc;

    { Internal handle to C API }
    property Handle: PCXIdxBaseClassInfo read FHandle;
  end;

type
  TIdxObjCProtocolRefInfo = record
  {$REGION 'Internal Declarations'}
  private
    FHandle: PCXIdxObjCProtocolRefInfo;
    function GetProtocol: TIdxEntityInfo; inline;
    function GetCursor: TCursor; inline;
    function GetLoc: TIdxLoc; inline;
  {$ENDREGION 'Internal Declarations'}
  public
    property Protocol: TIdxEntityInfo read GetProtocol;
    property Cursor: TCursor read GetCursor;
    property Loc: TIdxLoc read GetLoc;

    { Internal handle to C API }
    property Handle: PCXIdxObjCProtocolRefInfo read FHandle;
  end;

type
  TIdxObjCProtocolRefListInfo = record
  {$REGION 'Internal Declarations'}
  private
    FHandle: PCXIdxObjCProtocolRefListInfo;
    function GetProtocols: TArray<TIdxObjCProtocolRefInfo>;
  {$ENDREGION 'Internal Declarations'}
  public
    property Protocols: TArray<TIdxObjCProtocolRefInfo> read GetProtocols;

    { Internal handle to C API }
    property Handle: PCXIdxObjCProtocolRefListInfo read FHandle;
  end;

type
  TIdxObjCInterfaceDeclInfo = record
  {$REGION 'Internal Declarations'}
  private
    FHandle: PCXIdxObjCInterfaceDeclInfo;
    function GetContainerInfo: TIdxObjCContainerDeclInfo; inline;
    function GetSuperInfo: TIdxBaseClassInfo; inline;
    function GetProtocols: TIdxObjCProtocolRefListInfo; inline;
  {$ENDREGION 'Internal Declarations'}
  public
    property ContainerInfo: TIdxObjCContainerDeclInfo read GetContainerInfo;
    property SuperInfo: TIdxBaseClassInfo read GetSuperInfo;
    property Protocols: TIdxObjCProtocolRefListInfo read GetProtocols;

    { Internal handle to C API }
    property Handle: PCXIdxObjCInterfaceDeclInfo read FHandle;
  end;

type
  TIdxObjCCategoryDeclInfo = record
  {$REGION 'Internal Declarations'}
  private
    FHandle: PCXIdxObjCCategoryDeclInfo;
    function GetContainerInfo: TIdxObjCContainerDeclInfo; inline;
    function GetObjCClass: TIdxEntityInfo; inline;
    function GetClassCursor: TCursor; inline;
    function GetClassLoc: TIdxLoc; inline;
    function GetProtocols: TIdxObjCProtocolRefListInfo; inline;
  {$ENDREGION 'Internal Declarations'}
  public
    property ContainerInfo: TIdxObjCContainerDeclInfo read GetContainerInfo;
    property ObjCClass: TIdxEntityInfo read GetObjCClass;
    property ClassCursor: TCursor read GetClassCursor;
    property ClassLoc: TIdxLoc read GetClassLoc;
    property Protocols: TIdxObjCProtocolRefListInfo read GetProtocols;

    { Internal handle to C API }
    property Handle: PCXIdxObjCCategoryDeclInfo read FHandle;
  end;

type
  TIdxObjCPropertyDeclInfo = record
  {$REGION 'Internal Declarations'}
  private
    FHandle: PCXIdxObjCPropertyDeclInfo;
    function GetDeclInfo: TIdxDeclInfo; inline;
    function GetGetter: TIdxEntityInfo; inline;
    function GetSetter: TIdxEntityInfo; inline;
  {$ENDREGION 'Internal Declarations'}
  public
    property DeclInfo: TIdxDeclInfo read GetDeclInfo;
    property Getter: TIdxEntityInfo read GetGetter;
    property Setter: TIdxEntityInfo read GetSetter;

    { Internal handle to C API }
    property Handle: PCXIdxObjCPropertyDeclInfo read FHandle;
  end;

type
  TIdxIBOutletCollectionAttrInfo = record
  {$REGION 'Internal Declarations'}
  private
    FHandle: PCXIdxIBOutletCollectionAttrInfo;
    function GetAttrInfo: TIdxAttrInfo; inline;
    function GetObjCClass: TIdxEntityInfo; inline;
    function GetClassCursor: TCursor; inline;
    function GetClassLoc: TIdxLoc; inline;
  {$ENDREGION 'Internal Declarations'}
  public
    property AttrInfo: TIdxAttrInfo read GetAttrInfo;
    property ObjCClass: TIdxEntityInfo read GetObjCClass;
    property ClassCursor: TCursor read GetClassCursor;
    property ClassLoc: TIdxLoc read GetClassLoc;

    { Internal handle to C API }
    property Handle: PCXIdxIBOutletCollectionAttrInfo read FHandle;
  end;

type
  TIdxAttrInfoHelper = record helper for TIdxAttrInfo
  {$REGION 'Internal Declarations'}
  private
    function GetIBOutletCollectionAttrInfo: TIdxIBOutletCollectionAttrInfo; inline;
  {$ENDREGION 'Internal Declarations'}
  public
    property IBOutletCollectionAttrInfo: TIdxIBOutletCollectionAttrInfo read GetIBOutletCollectionAttrInfo;
  end;

type
  TIdxCxxClassDeclInfo = record
  {$REGION 'Internal Declarations'}
  private
    FHandle: PCXIdxCXXClassDeclInfo;
    function GetDeclInfo: TIdxDeclInfo; inline;
    function GetBases: TArray<TIdxBaseClassInfo>;
  {$ENDREGION 'Internal Declarations'}
  public
    property DeclInfo: TIdxDeclInfo read GetDeclInfo;
    property Bases: TArray<TIdxBaseClassInfo> read GetBases;

    { Internal handle to C API }
    property Handle: PCXIdxCXXClassDeclInfo read FHandle;
  end;

type
  TIdxDeclInfoHelper = record helper for TIdxDeclInfo
  {$REGION 'Internal Declarations'}
  private
    function GetObjCContainerDeclInfo: TIdxObjCContainerDeclInfo; inline;
    function GetObjCInterfaceDeclInfo: TIdxObjCInterfaceDeclInfo; inline;
    function GetObjCCategoryDeclInfo: TIdxObjCCategoryDeclInfo; inline;
    function GetObjCProtocolRefListInfo: TIdxObjCProtocolRefListInfo; inline;
    function GetObjCPropertyDeclInfo: TIdxObjCPropertyDeclInfo; inline;
    function GetCxxClassDeclInfo: TIdxCxxClassDeclInfo; inline;
  {$ENDREGION 'Internal Declarations'}
  public
    property ObjCContainerDeclInfo: TIdxObjCContainerDeclInfo read GetObjCContainerDeclInfo;
    property ObjCInterfaceDeclInfo: TIdxObjCInterfaceDeclInfo read GetObjCInterfaceDeclInfo;
    property ObjCCategoryDeclInfo: TIdxObjCCategoryDeclInfo read GetObjCCategoryDeclInfo;
    property ObjCProtocolRefListInfo: TIdxObjCProtocolRefListInfo read GetObjCProtocolRefListInfo;
    property ObjCPropertyDeclInfo: TIdxObjCPropertyDeclInfo read GetObjCPropertyDeclInfo;
    property CxxClassDeclInfo: TIdxCxxClassDeclInfo read GetCxxClassDeclInfo;
  end;

type
  { Data for IIndexerListener.IndexEntityReference.

    This may be deprecated in a future version as this duplicates the
    TSymbolRole.Implicit flag. }
  TIdxEntityRefKind = (
    { The entity is referenced directly in user's code. }
    Direct = CXIdxEntityRef_Direct,

    { An implicit reference, e.g. a reference of an Objective-C method via the
      dot syntax. }
    Implicit = CXIdxEntityRef_Implicit);

type
  { Roles that are attributed to symbol occurrences. }
  TSymbolRole = (
    Declaration,
    Definition,
    Reference,
    Read,
    Write,
    Call,
    Dynamic,
    AddressOf,
    Implicit);
  TSymbolRoles = set of TSymbolRole;

type
  { Data for IIndexerListener.IndexEntityReference. }
  TIdxEntityRefInfo = record
  {$REGION 'Internal Declarations'}
  private
    FHandle: PCXIdxEntityRefInfo;
    function GetKind: TIdxEntityRefKind; inline;
    function GetCursor: TCursor; inline;
    function GetLoc: TIdxLoc; inline;
    function GetReferencedEntity: TIdxEntityInfo; inline;
    function GetParentEntity: TIdxEntityInfo; inline;
    function GetContainer: TIdxContainerInfo; inline;
    function GetRoles: TSymbolRoles; inline;
  {$ENDREGION 'Internal Declarations'}
  public
    property Kind: TIdxEntityRefKind read GetKind;

    { Reference cursor. }
    property Cursor: TCursor read GetCursor;

    property Loc: TIdxLoc read GetLoc;

    { The entity that gets referenced. }
    property ReferencedEntity: TIdxEntityInfo read GetReferencedEntity;

    { Immediate "parent" of the reference. For example:

      @preformatted(
        Foo *var;
      )

      The parent of reference of type 'Foo' is the variable 'var'. For
      references inside statement bodies of functions/methods, the ParentEntity
      will be the function/method. }
    property ParentEntity: TIdxEntityInfo read GetParentEntity;

    { Lexical container context of the reference. }
    property Container: TIdxContainerInfo read GetContainer;

    { Sets of symbol roles of the reference. }
    property Roles: TSymbolRoles read GetRoles;

    { Internal handle to C API }
    property Handle: PCXIdxEntityRefInfo read FHandle;
  end;

type
  { A group of callbacks used by IIndexAction.IndexSourceFile and
    IIndexAction.IndexTranslationUnit.
    Implement this interface to provide these callbacks. }
  IIndexerListener = interface
  ['{D585B3D2-E1AC-4E0D-A7FE-FF21DED1F7DB}']
    { Called periodically to check whether indexing should be aborted.
      Should return False to continue, and True to abort. }
    function AbortQuery(const AReserved: Pointer): Boolean;

    { Called at the end of indexing; passes the complete diagnostic set. }
    procedure Diagnostic(const ADiagnostics: IDiagnosticSet;
      const AReserved: Pointer);

    function EnteredMainFile(const AMainFile: TFile;
      const AReserved: Pointer): TIdxClientFile;

    { Called when a file gets #included/#imported.}
    function IncludedFile(const AInfo: TIdxIncludedFileInfo): TIdxClientFile;

    { Called when a AST file (PCH or module) gets imported.

      AST files will not get indexed (there will not be callbacks to index all
      the entities in an AST file). The recommended action is that, if the AST
      file is not already indexed, to initiate a new indexing job specific to
      the AST file. }
    function ImportedAstFile(const AInfo: TIdxImportedAstFileInfo): TIdxClientFile;

    { Called at the beginning of indexing a translation unit. }
    function StartedTranslationUnit(const AReserved: Pointer): TIdxClientContainer;

    procedure IndexDeclaration(const AInfo: TIdxDeclInfo);

    { Called to index a reference of an entity. }
    procedure IndexEntityReference(const AInfo: TIdxEntityRefInfo);
  end;

type
  { An indexing action/session, to be applied to one or multiple
    translation units. }
  IIndexAction = interface
  ['{E5988F80-CA12-4EBA-B4E9-2B78EE81A5D9}']
    {$REGION 'Internal Declarations'}
    function GetHandle: TCXIndexAction;
    {$ENDREGION 'Internal Declarations'}

    { Index the given source file and the translation unit corresponding to that
      file via callbacks implemented through IIndexerListener.

      Parameters:
        AListener: indexing callbacks that the client implements.
        AOptions: options that affects how indexing is performed.
        ASourceFilename: The name of the source file to load, or an empty string
          if the source file is included in AClangCommandLineArgs.
        AClangCommandLineArgs: (optional) command-line arguments that would be
          passed to the clang executable if it were being invoked out-of-process.
          These command-line options will be parsed and will affect how the
          translation unit is parsed. Note that the following options are
          ignored: '-c', '-emit-ast', '-fsyntax-only' (which is the default),
          and '-o <output file>'.
        AUnsavedFiles: (optional) array of files that have not yet been saved to
          disk but may be required for code completion, including the contents
          of those files.
        ATUOptions: (optional) options that affects how the translation unit is
          managed but not its compilation.
        ATranslationUnit: (optional) is set to a translation unit that can be
          reused after indexing is finished.

      Returns:
        Error code. If there were errors from which the compiler could recover,
        then this is not reported as an error. Only if there is a failure from
        which there is no recovery, will an error be returned. }
    function IndexSourceFile(const AListener: IIndexerListener;
      const AOptions: TIndexOptions; const ASourceFilename: String;
      const AClangCommandLineArgs: array of String;
      const AUnsavedFiles: TArray<TUnsavedFile> = nil;
      const ATUOptions: TTranslationUnitFlags = []): TError; overload;
    function IndexSourceFile(const AListener: IIndexerListener;
      const AOptions: TIndexOptions; const ASourceFilename: String;
      const AClangCommandLineArgs: array of String;
      const AUnsavedFiles: TArray<TUnsavedFile>;
      const ATUOptions: TTranslationUnitFlags;
      out ATranslationUnit: ITranslationUnit): TError; overload;

    { Index the given translation unit via callbacks implemented through
      IndexerListener.

      Parameters:
        AListener: indexing callbacks that the client implements.
        AOptions: options that affects how indexing is performed.
        ATranslationUnit: the translation unit to index.

      Returns:
        Error code. If there were errors from which the compiler could recover,
        then this is not reported as an error. Only if there is a failure from
        which there is no recovery, will an error be returned.

      The order of callback invocations is not guaranteed to be the same as
      when indexing a source file. The high level order will be:
      * Preprocessor callbacks invocations
      * Declaration/reference callbacks invocations
      * Diagnostic callback invocations }
    function IndexTranslationUnit(const AListener: IIndexerListener;
      const AOptions: TIndexOptions; const ATranslationUnit: ITranslationUnit): TError;

    { Internal handle to C API }
    property Handle: TCXIndexAction read GetHandle;
  end;

type
  { An "index" that consists of a set of translation units that would
    typically be linked together into an executable or library.
    Use TIndex to create an instance. }
  IIndex = interface
  ['{F432C0B8-2E5F-459E-8514-BE601317F6F3}']
    {$REGION 'Internal Declarations'}
    function GetGlobalOptions: TGlobalOptions;
    procedure SetGlobalOptions(const AValue: TGlobalOptions);
    procedure SetInvocationEmissionPath(const AValue: String);
    function GetHandle: TCXIndex;
    {$ENDREGION 'Internal Declarations'}

    { Return the ITranslationUnit for a given source file and the provided
      command line arguments one would pass to the compiler.

      Parameters:
        ASourceFilename: The name of the source file to load, or an empty string
          if the source file is included in AClangCommandLineArgs.
        AClangCommandLineArgs: (optional) command-line arguments that would be
          passed to the clang executable if it were being invoked out-of-process.
          These command-line options will be parsed and will affect how the
          translation unit is parsed. Note that the following options are
          ignored: '-c', '-emit-ast', '-fsyntax-only' (which is the default),
          and '-o <output file>'.
        AUnsavedFiles: (optional) array of files that have not yet been saved to
          disk but may be required for code completion, including the contents
          of those files.

      Returns:
        The translation unit or nil in case of an error (eg. file does not
        exist or parse error)

      Note: The ASourceFilename argument is optional. If the caller provides an
      empty string, the name of the source file is expected to reside in the
      specified command line arguments.

      Note: When encountered in AClangCommandLineArgs, the following options
      are ignored:

      * '-c'
      * '-emit-ast'
      * '-fsyntax-only'
      * '-o <output file>'  (both '-o' and '<output file>' are ignored) }
    function CreateTranslationUnitFromSource(const ASourceFilename: String;
      const AClangCommandLineArgs: array of String;
      const AUnsavedFiles: TArray<TUnsavedFile> = nil): ITranslationUnit; overload;
    function CreateTranslationUnitFromSource(const ASourceFilename: String;
      const AUnsavedFiles: TArray<TUnsavedFile> = nil): ITranslationUnit; overload;

    { Create a translation unit from an AST file (-emit-ast).

      Parameters:
        AAstFilename: name of the AST file to load.

      Returns:
        The translation unit or nil in case of an error (eg. file does not
        exist or in invalid format) }
    function CreateTranslationUnit(const AAstFilename: String): ITranslationUnit;

    { Parse the given source file and the translation unit corresponding to that
      file.

      This routine is the main entry point for the Clang C API, providing the
      ability to parse a source file into a translation unit that can then be
      queried by other functions in the API. This routine accepts a set of
      command-line arguments so that the compilation can be configured in the
      same way that the compiler is configured on the command line.

      Parameters:
        ASourceFilename: The name of the source file to load, or an empty string
          if the source file is included in AClangCommandLineArgs.
        AClangCommandLineArgs: (optional) command-line arguments that would be
          passed to the clang executable if it were being invoked out-of-process.
          These command-line options will be parsed and will affect how the
          translation unit is parsed. Note that the following options are
          ignored: '-c', '-emit-ast', '-fsyntax-only' (which is the default),
          and '-o <output file>'.
        AUnsavedFiles: (optional) array of files that have not yet been saved to
          disk but may be required for code completion, including the contents
          of those files.
        AOptions: (optional) options that affects how the translation unit is
          managed but not its compilation.

      Returns:
        The translation unit or nil in case of an error (eg. file does not
        exist or parse error) }
    function ParseTranslationUnit(const ASourceFilename: String;
      const AClangCommandLineArgs: array of String;
      const AUnsavedFiles: TArray<TUnsavedFile> = nil;
      const AOptions: TTranslationUnitFlags = []): ITranslationUnit;

    { Create an indexing action/session, to be applied to one or multiple
      translation units.

      Returns:
        The index action or nil in case of an error. }
    function CreateIndexAction: IIndexAction;

    { General options associated with an Index }
    property GlobalOptions: TGlobalOptions read GetGlobalOptions write SetGlobalOptions;

    { The invocation emission path. Specifies a path which will contain log
      files for certain libclang invocations. An empty string (default) implies
      that libclang invocations are not logged. }
    property InvocationEmissionPath: String write SetInvocationEmissionPath;

    { Internal handle to C API }
    property Handle: TCXIndex read GetHandle;
  end;

type
  { Represents the command line invocation to compile a specific file. }
  TCompileCommand = record
  {$REGION 'Internal Declarations'}
  private
    FHandle: TCXCompileCommand;
    function GetDirectory: String; inline;
    function GetFilename: String; inline;
    function GetArgCount: Integer; inline;
    function GetArg(const AIndex: Integer): String; inline;
    function GetMappedSourceCount: Integer; inline;
    function GetMappedSourcePath(const AIndex: Integer): String; inline;
    function GetMappedSourceContent(const AIndex: Integer): String; inline;
  {$ENDREGION 'Internal Declarations'}
  public
    { The working directory where the CompileCommand was executed from }
    property Directory: String read GetDirectory;

    { The filename associated with the CompileCommand. }
    property Filename: String read GetFilename;

    { The number of arguments in the compiler invocation. }
    property ArgCount: Integer read GetArgCount;

    { The arguments in the compiler invocation. }
    property Args[const AIndex: Integer]: String read GetArg;

    { The number of source mappings for the compiler invocation. }
    property MappedSourceCount: Integer read GetMappedSourceCount;

    { The mapped source paths for the compiler invocation. }
    property MappedSourcePaths[const AIndex: Integer]: String read GetMappedSourcePath;

    { The mapped source contents for the compiler invocation. }
    property MappedSourceContents[const AIndex: Integer]: String read GetMappedSourceContent;

    { Internal handle to C API }
    property Handle: TCXCompileCommand read FHandle;
  end;

type
  { Contains the results of a search in the compilation database.

    When searching for the compile command for a file, the compilation db can
    return several commands, as the file may have been compiled with different
    options in different places of the project. This choice of compile commands
    is wrapped in this opaque data structure.}
  ICompileCommands = interface
  ['{821BBBB9-D1EE-4F60-83B7-9066FFEE4067}']
    {$REGION 'Internal Declarations'}
    function GetCount: Integer;
    function GetCommand(const AIndex: Integer): TCompileCommand;
    function GetHandle: TCXCompileCommands;
    {$ENDREGION 'Internal Declarations'}

    { The number of compile commands we have for a file }
    property Count: Integer read GetCount;

    { The compile commands }
    property Commands[const AIndex: Integer]: TCompileCommand read GetCommand;

    { Internal handle to C API }
    property Handle: TCXCompileCommands read GetHandle;
  end;

type
  { A compilation database holds all information used to compile files in a
    project. For each file in the database, it can be queried for the working
    directory or the command line used for the compiler invocation. }
  ICompilationDatabase = interface
  ['{4D2E95DE-E59B-43E0-B76D-8E65D4875310}']
    {$REGION 'Internal Declarations'}
    function GetHandle: TCXCompilationDatabase;
    {$ENDREGION 'Internal Declarations'}

    { Find the compile commands used for a file.  }
    function GetCompileCommands(const ACompleteFilename: String): ICompileCommands;

    { Get all the compile commands in the given compilation database. }
    function GetAllCompileCommands: ICompileCommands;

    { Internal handle to C API }
    property Handle: TCXCompilationDatabase read GetHandle;
  end;

type
  { Object encapsulating information about overlaying virtual file/directories
    over the real file system. }
  IVirtualFileOverlay = interface
  ['{9FE70505-90FC-475F-A436-386D68399A5D}']
    {$REGION 'Internal Declarations'}
    function GetHandle: TCXVirtualFileOverlay;
    {$ENDREGION 'Internal Declarations'}

    { Map an absolute virtual file path to an absolute real one. The virtual
      path must be canonicalized (not contain "."/".."). }
    function AddFileMapping(const AVirtualPath, ARealPath: String): TError;

    { Set the case sensitivity. The virtual file overlay object is
      case-sensitive by default, but this option can be used to override the
      default. }
    function SetCaseSensitivity(const ACaseSensitive: Boolean): TError;

    { Write out the virtual file overlay object to a byte buffer. }
    function WriteToBuffer(out ABuffer: TBytes): TError;

    { Internal handle to C API }
    property Handle: TCXVirtualFileOverlay read GetHandle;
  end;

type
  { Object encapsulating information about a module.map file. }
  IModuleMapDescriptor = interface
  ['{4A0E063B-1438-4D74-90CC-54A7B5C80C85}']
    {$REGION 'Internal Declarations'}
    function GetHandle: TCXModuleMapDescriptor;
    {$ENDREGION 'Internal Declarations'}

    { Sets the framework module name that the module.map describes. }
    function SetFrameworkModuleName(const AName: String): TError;

    { Sets the umbrealla header name that the module.map describes. }
    function SetUmbrellaHeader(const AName: String): TError;

    { Write out the descriptor object to a byte buffer. }
    function WriteToBuffer(out ABuffer: TBytes): TError;

    { Internal handle to C API }
    property Handle: TCXModuleMapDescriptor read GetHandle;
  end;

type
  { A remapping of original source files and their translated files.
    Implemented by TRemapping. }
  IRemapping = interface
  ['{7F1E48C4-2937-4BDE-AF40-E4980B08043D}']
    {$REGION 'Internal Declarations'}
    function GetCount: Integer;
    function GetOriginalFilename(const AIndex: Integer): String;
    function GetAssociatedFilename(const AIndex: Integer): String;
    function GetHandle: TCXRemapping;
    {$ENDREGION 'Internal Declarations'}

    { The number of remappings. }
    property Count: Integer read GetCount;

    { The original filenames from the remapping. }
    property OriginalFilenames[const AIndex: Integer]: String read GetOriginalFilename;

    { The associated filename from the remapping. }
    property AssociatedFilenames[const AIndex: Integer]: String read GetAssociatedFilename;

    { Internal handle to C API }
    property Handle: TCXRemapping read GetHandle;
  end;

type
  { Implements IIndex }
  TIndex = class(TInterfacedObject, IIndex)
  {$REGION 'Internal Declarations'}
  private
    FHandle: TCXIndex;
  private
    function DoCreateTranslationUnit(const ASourceFilename: String;
      const AClangCommandLineArgs: array of String;
      const AUnsavedFiles: TArray<TUnsavedFile>;
      const AOptions: TTranslationUnitFlags;
      const AParse: Boolean): ITranslationUnit;
  protected
    { IIndex }
    function GetGlobalOptions: TGlobalOptions;
    procedure SetGlobalOptions(const AValue: TGlobalOptions);
    procedure SetInvocationEmissionPath(const AValue: String);
    function GetHandle: TCXIndex;

    function CreateTranslationUnitFromSource(const ASourceFilename: String;
      const AClangCommandLineArgs: array of String;
      const AUnsavedFiles: TArray<TUnsavedFile>): ITranslationUnit; overload;
    function CreateTranslationUnitFromSource(const ASourceFilename: String;
      const AUnsavedFiles: TArray<TUnsavedFile>): ITranslationUnit; overload;
    function CreateTranslationUnit(const AAstFilename: String): ITranslationUnit;
    function ParseTranslationUnit(const ASourceFilename: String;
      const AClangCommandLineArgs: array of String;
      const AUnsavedFiles: TArray<TUnsavedFile>;
      const AOptions: TTranslationUnitFlags): ITranslationUnit;
    function CreateIndexAction: IIndexAction;
  {$ENDREGION 'Internal Declarations'}
  public
    { Provides a shared context for creating translation units.

      Parameters:
        AExcludeDeclarationsFromPCH: When True, allows enumeration of "local"
          declarations (when loading any new translation units). A "local"
          declaration is one that belongs in the translation unit itself and not
          in a precompiled header that was used by the translation unit. If
          False, all declarations will be enumerated.
        ADisplayDiagnostics: TBD

      Example:

      <source>
      var
        Idx: IIndex;
        TU: ITranslationUnit;
      begin
        Idx := TIndex.Create(True, True);

        // IndexTest.pch was produced with the following command:
        // "clang -x c IndexTest.h -emit-ast -o IndexTest.pch"
        TU := Idx.CreateTranslationUnit('IndexTest.pch');

        // This will load all the symbols from 'IndexTest.pch'
        TU.Cursor.VisitChildren(MyVisitor);

        // This will load all the symbols from 'IndexTest.c', excluding symbols
        // from 'IndexTest.pch'.
        TU := Idx.CreateTranslationUnitFromSource('IndexTest.c', ['-Xclang',
          '-include-pch=IndexTest.pch']);
        TU.Cursor.VisitChildren(MyVisitor);
      end;
      </source>

      This process of creating the 'pch', loading it separately, and using it
      (via -include-pch) allows 'AExcludeDeclsFromPCH' to remove redundant
      callbacks (which gives the indexer the same performance benefit as the
      compiler). }
    constructor Create(const AExcludeDeclarationsFromPCH,
      ADisplayDiagnostics: Boolean);
    destructor Destroy; override;
  end;

type
  { Implements ICompilationDatabase }
  TCompilationDatabase = class(TInterfacedObject, ICompilationDatabase)
  {$REGION 'Internal Declarations'}
  private
    FHandle: TCXCompilationDatabase;
  protected
    { ICompilationDatabase }
    function GetHandle: TCXCompilationDatabase;

    function GetCompileCommands(const ACompleteFilename: String): ICompileCommands;
    function GetAllCompileCommands: ICompileCommands;
  public
    constructor Create(const AHandle: TCXCompilationDatabase);
  {$ENDREGION 'Internal Declarations'}
  public
    { Creates a compilation database from the database found in directory
      ABuildDir. For example, CMake can output a compile_commands.json which can
      be used to build the database.

      Returns nil if the database cannot be loaded. }
    class function FromDirectory(const ABuildDir: String): ICompilationDatabase; static;

    destructor Destroy; override;
  end;

type
  { Implements IVirtualFileOverlay }
  TVirtualFileOverlay = class(TInterfacedObject, IVirtualFileOverlay)
  {$REGION 'Internal Declarations'}
  private
    FHandle: TCXVirtualFileOverlay;
  protected
    { Declarations }
    function GetHandle: TCXVirtualFileOverlay;

    function AddFileMapping(const AVirtualPath, ARealPath: String): TError;
    function SetCaseSensitivity(const ACaseSensitive: Boolean): TError;
    function WriteToBuffer(out ABuffer: TBytes): TError;
  {$ENDREGION 'Internal Declarations'}
  public
    { Create an IVirtualFileOverlay object }
    constructor Create;

    destructor Destroy; override;
  end;

type
  { Implements IModuleMapDescriptor }
  TModuleMapDescriptor = class(TInterfacedObject, IModuleMapDescriptor)
  {$REGION 'Internal Declarations'}
  private
    FHandle: TCXModuleMapDescriptor;
  protected
    { IModuleMapDescriptor }
    function GetHandle: TCXModuleMapDescriptor;

    function SetFrameworkModuleName(const AName: String): TError;
    function SetUmbrellaHeader(const AName: String): TError;
    function WriteToBuffer(out ABuffer: TBytes): TError;
  {$ENDREGION 'Internal Declarations'}
  public
    { Create an IModuleMapDescriptor object }
    constructor Create;

    destructor Destroy; override;
  end;

type
  { Implements IDiagnosticSet }
  TDiagnosticSet = class(TInterfacedObject, IDiagnosticSet)
  {$REGION 'Internal Declarations'}
  private
    FHandle: TCXDiagnosticSet;
    FOwnsHandle: Boolean;
  protected
    { IDiagnosticSet }
    function GetCount: Integer;
    function GetDiagnostic(const AIndex: Integer): IDiagnostic;
    function GetHandle: TCXDiagnosticSet;
  public
    constructor Create(const AHandle: TCXDiagnosticSet;
      const AOwnsHandle: Boolean = True);
  {$ENDREGION 'Internal Declarations'}
  public
    { Deserialize a set of diagnostics from a Clang diagnostics bitcode file.

      Parameters:
        AFilename: The name of the file to deserialize.
        AError: (optional) is set to an error code (if any).
        AErrorString: (optional) is set to an error string (if any).

      Returns:
        The loaded diagnostic set, of nil on error. }
    function Load(const AFilename: String): IDiagnosticSet; overload;
    function Load(const AFilename: String; out AError: TLoadDiagError;
      out AErrorString: String): IDiagnosticSet; overload;

    destructor Destroy; override;
  end;

type
  { Implements ICursorSet }
  TCursorSet = class(TInterfacedObject, ICursorSet)
  {$REGION 'Internal Declarations'}
  private
    FHandle: TCXCursorSet;
  protected
    { ICursorSet }
    function GetHandle: TCXCursorSet;

    function Contains(const ACursor: TCursor): Boolean;
    function Insert(const ACursor: TCursor): Boolean;
  {$ENDREGION 'Internal Declarations'}
  public
    { Creates an empty ICursorSet }
    constructor Create;
    destructor Destroy; override;
  end;

type
  { Implements IRemapping }
  TRemapping = class(TInterfacedObject, IRemapping)
  {$REGION 'Internal Declarations'}
  private
    FHandle: TCXRemapping;
  protected
    { IRemapping }
    function GetCount: Integer;
    function GetOriginalFilename(const AIndex: Integer): String;
    function GetAssociatedFilename(const AIndex: Integer): String;
    function GetHandle: TCXRemapping;
  public
    constructor Create(const AHandle: TCXRemapping); overload;
  {$ENDREGION 'Internal Declarations'}
  public
    { Retrieve a remapping.

      Parameters:
        APath: the path that contains metadata about remappings.

      Returns:
        The requested remapping. Can return nil if an error occurred. }
    class function Create(const APath: String): IRemapping; overload; static;

    { Retrieve a remapping.

      Parameters:
        AFilePaths: array of file paths containing remapping info.

      Returns:
        The requested remapping. Can return nil if an error occurred. }
    class function Create(const AFilePaths: array of String): IRemapping; overload; static;

    destructor Destroy; override;
  end;

{ Return a version string, suitable for showing to a user, but not intended to
  be parsed (the format is not guaranteed to be stable). }
function GetClangVersion: String; inline;

{ Enable/disable crash recovery. }
procedure ToggleCrashRecovery(const AEnable: Boolean); inline;

{ Return the timestamp for use with Clang's -fbuild-session-timestamp= option. }
function GetBuildSessionTimestamp: UInt64; inline;

{ The set of display options most similar to the default behavior of the clang
  compiler. }
function GetDefaultDiagnosticDisplayOptions: TDiagnosticDisplayOptions; inline;

{ Returns a default set of code-completion options that can be passed to
  ITranslationUnit.CodeCompleteAt. }
function GetDefaultCodeCompleteOptions: TCodeCompleteFlags; inline;

{ Construct a USR for a specified Objective-C class. }
function ConstructUsrForObjCClass(const AClassName: String): TUnifiedSymbolResolution; inline;

{ Construct a USR for a specified Objective-C category. }
function ConstructUsrForObjCCategory(const AClassName, ACategoryName: String): TUnifiedSymbolResolution; inline;

{ Construct a USR for a specified Objective-C protocol. }
function ConstructUsrForObjCProtocol(const AProtocolName: String): TUnifiedSymbolResolution; inline;

{ Construct a USR for a specified Objective-C instance variable and the USR for
  its containing class. }
function ConstructUsrForObjCIVar(const AName: String;
  const AClassUsr: TUnifiedSymbolResolution): TUnifiedSymbolResolution; inline;

{ Construct a USR for a specified Objective-C method and the USR for its
  containing class. }
function ConstructUsrForObjCMethod(const AName: String;
  const AIsInstanceMethod: Boolean;
  const AClassUsr: TUnifiedSymbolResolution): TUnifiedSymbolResolution; inline;

{ Construct a USR for a specified Objective-C property and the USR for its
  containing class. }
function ConstructUsrForObjCProperty(const AProperty: String;
  const AClassUsr: TUnifiedSymbolResolution): TUnifiedSymbolResolution; inline;

{ Converts a clang-string to a Delphi string. Used internally.

  Parameters:
    ASource: the source clang-string.
    ADispose: (optional) whether to dispose the original clang-string. Defaults
      to True.

  Returns:
    The corresponding Delphi string. }
function CXStringToString(const ASource: TCXString; const ADispose: Boolean = True): String;

implementation

uses
  System.DateUtils;

{$POINTERMATH ON}

type
  TInclusionVisitorProc = record
    Anon: TInclusionVisitor;
    Meth: TInclusionVisitorMethod;
  end;
  PInclusionVisitorProc = ^TInclusionVisitorProc;

type
  TCursorAndRangeVisitorProc = record
    Anon: TCursorAndRangeVisitor;
    Meth: TCursorAndRangeVisitorMethod;
  end;
  PCursorAndRangeVisitorProc = ^TCursorAndRangeVisitorProc;

type
  TCursorVisitorProc = record
    Anon: TCursorVisitor;
    Meth: TCursorVisitorMethod;
  end;
  PCursorVisitorProc = ^TCursorVisitorProc;

type
  TFieldVisitorProc = record
    Anon: TFieldVisitor;
    Meth: TFieldVisitorMethod;
  end;
  PFieldVisitorProc = ^TFieldVisitorProc;

type
  TTranslationUnit = class(TInterfacedObject, ITranslationUnit)
  {$REGION 'Internal Declarations'}
  private
    FHandle: TCXTranslationUnit;
  private
    class procedure InclusionVisitor(AIncludedFile: TCXFile;
      AInclusionStack: PCXSourceLocation; AIncludeLen: Cardinal;
      AClientData: TCXClientData); cdecl; static;
  protected
    { ITranslationUnit }
    function GetDiagnosticCount: Integer;
    function GetDiagnostic(const AIndex: Integer): IDiagnostic;
    function GetAllDiagnostics: IDiagnosticSet;
    function GetSpelling: String;
    function GetResourceUsage: IResourceUsage;
    function GetTargetInfo: ITargetInfo;
    function GetCursor: TCursor; overload;
    function GetHandle: TCXTranslationUnit;

    function GetFile(const AFilename: String): TFile;
    function GetFileContents(const AFile: TFile): TBytes;
    function IsFileMultipleIncludeGuarded(const AFile: TFile): Boolean;
    function GetLocation(const AFile: TFile; const ALine,
      AColumn: Integer): TSourceLocation; overload;
    function GetLocation(const AFile: TFile;
      const AOffset: Integer): TSourceLocation; overload;
    function GetSkippedRanges(const AFile: TFile): ISourceRangeList;
    function GetAllSkippedRanges: ISourceRangeList;
    function Save(const AFilename: String): TSaveResult;
    procedure Suspend;
    function Reparse(const AUnsavedFiles: TArray<TUnsavedFile>): TError;
    function GetCursor(const ASourceLocation: TSourceLocation): TCursor; overload;
    function GetModule(const AFile: TFile): TModule;
    function GetTopLevelHeaderCount(const AModule: TModule): Integer;
    function GetTopLevelHeader(const AModule: TModule; const AIndex: Integer): TFile;
    function Tokenize(const ARange: TSourceRange): ITokenList;
    function AnnotateTokens(const ATokens: ITokenList): TArray<TCursor>; overload;
    function AnnotateTokens(const ATokens: TArray<TToken>): TArray<TCursor>; overload;
    function GetTokenSpelling(const AToken: TToken): String;
    function GetTokenLocation(const AToken: TToken): TSourceLocation;
    function GetTokenExtent(const AToken: TToken): TSourceRange;
    function CodeCompleteAt(const ACompleteFilename: String; const ACompleteLine,
      ACompleteColumn: Integer; const AUnsavedFiles: TArray<TUnsavedFile> = nil;
      const AOptions: TCodeCompleteFlags = []): ICodeCompleteResults;
    procedure GetInclusions(const AVisitor: TInclusionVisitor); overload;
    procedure GetInclusions(const AVisitor: TInclusionVisitorMethod); overload;
    function FindIncludesInFile(const AFile: TFile;
      const AVisitor: TCursorAndRangeVisitor): TVisitResult; overload;
    function FindIncludesInFile(const AFile: TFile;
      const AVisitor: TCursorAndRangeVisitorMethod): TVisitResult; overload;
    function GetToken(const ALocation: TSourceLocation): ITokenList;
  public
    constructor Create(const AHandle: TCXTranslationUnit);
  {$ENDREGION 'Internal Declarations'}
  public
    destructor Destroy; override;
  end;

type
  TIndexAction = class(TInterfacedObject, IIndexAction)
  {$REGION 'Internal Declarations'}
  private
    FHandle: TCXIndexAction;
  private
    class procedure SetupCallbacks(out ACallbacks: TIndexerCallbacks); static;
    class function CallbackAbortQuery(AClientData: TCXClientData;
      AReserved: Pointer): Integer; cdecl; static;
    class procedure CallbackDiagnostic(AClientData: TCXClientData;
      ADiagnostics: TCXDiagnosticSet; AReserved: Pointer); cdecl; static;
    class function CallbackEnteredMainFile(AClientData: TCXClientData;
      AMainFile: TCXFile; AReserved: Pointer): TCXIdxClientFile; cdecl; static;
    class function CallbackIncludedFile(AClientData: TCXClientData;
      const AInfo: PCXIdxIncludedFileInfo): TCXIdxClientFile; cdecl; static;
    class function CallbackImportedAstFile(AClientData: TCXClientData;
      const AInfo: PCXIdxImportedASTFileInfo): TCXIdxClientASTFile; cdecl; static;
    class function CallbackStartedTranslationUnit(AClientData: TCXClientData;
      AReserved: Pointer): TCXIdxClientContainer; cdecl; static;
    class procedure CallbackIndexDeclaration(AClientData: TCXClientData;
      const AInfo: PCXIdxDeclInfo); cdecl; static;
    class procedure CallbackIndexEntityReference(AClientData: TCXClientData;
      const AInfo: PCXIdxEntityRefInfo); cdecl; static;
  protected
    { IIndexAction }
    function GetHandle: TCXIndexAction;

    function IndexSourceFile(const AListener: IIndexerListener;
      const AOptions: TIndexOptions; const ASourceFilename: String;
      const AClangCommandLineArgs: array of String;
      const AUnsavedFiles: TArray<TUnsavedFile>;
      const ATUOptions: TTranslationUnitFlags): TError; overload;
    function IndexSourceFile(const AListener: IIndexerListener;
      const AOptions: TIndexOptions; const ASourceFilename: String;
      const AClangCommandLineArgs: array of String;
      const AUnsavedFiles: TArray<TUnsavedFile>;
      const ATUOptions: TTranslationUnitFlags;
      out ATranslationUnit: ITranslationUnit): TError; overload;
    function IndexTranslationUnit(const AListener: IIndexerListener;
      const AOptions: TIndexOptions; const ATranslationUnit: ITranslationUnit): TError;
  public
    constructor Create(const AHandle: TCXIndexAction);
  {$ENDREGION 'Internal Declarations'}
  public
    destructor Destroy; override;
  end;

type
  TDiagnostic = class(TInterfacedObject, IDiagnostic)
  {$REGION 'Internal Declarations'}
  private
    FHandle: TCXDiagnostic;
  protected
    { IDiagnostic }
    function GetChildDiagnostics: IDiagnosticSet;
    function GetSeverity: TDiagnosticSeverity;
    function GetLocation: TSourceLocation;
    function GetSpelling: String;
    function GetEnableOption: String;
    function GetDisableOption: String;
    function GetCategory: Integer;
    function GetCategoryText: String;
    function GetRangeCount: Integer;
    function GetRange(const AIndex: Integer): TSourceRange;
    function GetFixItCount: Integer;
    function GetHandle: TCXDiagnostic;

    function Format(const AOptions: TDiagnosticDisplayOptions): String;
    function FixIt(const AFixItIndex: Integer;
      out AReplacementRange: TSourceRange): String;
  public
    constructor Create(const AHandle: TCXDiagnostic);
  {$ENDREGION 'Internal Declarations'}
  public
    destructor Destroy; override;
  end;

type
  TResourceUsage = class(TInterfacedObject, IResourceUsage)
  {$REGION 'Internal Declarations'}
  private
    FHandle: TCXTUResourceUsage;
  protected
    { IResourceUsage }
    function GetHandle: TCXTUResourceUsage;
  public
    constructor Create(const AHandle: TCXTUResourceUsage);
  {$ENDREGION 'Internal Declarations'}
  public
    destructor Destroy; override;
  end;

type
  TTargetInfo = class(TInterfacedObject, ITargetInfo)
  {$REGION 'Internal Declarations'}
  private
    FHandle: TCXTargetInfo;
  protected
    { ITargetInfo }
    function GetTriple: String;
    function GetPointerWidth: Integer;
    function GetHandle: TCXTargetInfo;
  public
    constructor Create(const AHandle: TCXTargetInfo);
  {$ENDREGION 'Internal Declarations'}
  public
    destructor Destroy; override;
  end;

type
  TTokenList = class(TInterfacedObject, ITokenList)
  {$REGION 'Internal Declarations'}
  private
    FTranslationUnit: ITranslationUnit;
    FTokens: PCXToken;
    FCount: Integer;
  protected
    { ITokenList }
    function GetCount: Integer;
    function GetToken(const AIndex: Integer): TToken;
  public
    constructor Create(const ATranslationUnit: ITranslationUnit;
      const ATokens: PCXToken; const ACount: Integer);
  {$ENDREGION 'Internal Declarations'}
  public
    destructor Destroy; override;
  end;

type
  TCodeCompleteResults = class(TInterfacedObject, ICodeCompleteResults)
  {$REGION 'Internal Declarations'}
  private
    FHandle: PCXCodeCompleteResults;
  protected
    { ICodeCompleteResults }
    function GetCount: Integer;
    function GetResult(const AIndex: Integer): TCompletionResult;
    function GetDiagnosticCount: Integer;
    function GetDiagnostic(const AIndex: Integer): IDiagnostic;
    function GetContexts: UInt64;
    function GetContainerKind: TCursorKind;
    function GetContainerComplete: Boolean;
    function GetContainerUsr: TUnifiedSymbolResolution;
    function GetObjCSelector: String;
    function GetFixItCount(const AIndex: Integer): Integer;
    function GetHandle: PCXCodeCompleteResults;

    function GetCompletionFixIt(const AIndex, AFixItIndex: Integer;
      const AReplacementRange: TSourceRange): String;
  public
    constructor Create(const AHandle: PCXCodeCompleteResults);
  {$ENDREGION 'Internal Declarations'}
  public
    destructor Destroy; override;
  end;

type
  TCompileCommands = class(TInterfacedObject, ICompileCommands)
  {$REGION 'Internal Declarations'}
  private
    FHandle: TCXCompileCommands;
  protected
    { ICompileCommands }
    function GetCount: Integer;
    function GetCommand(const AIndex: Integer): TCompileCommand;
    function GetHandle: TCXCompileCommands;
  public
    constructor Create(const AHandle: TCXCompileCommands);
  {$ENDREGION 'Internal Declarations'}
  public
    destructor Destroy; override;
  end;

type
  TSourceRangeList = class(TInterfacedObject, ISourceRangeList)
  {$REGION 'Internal Declarations'}
  private
    FHandle: PCXSourceRangeList;
  protected
    { ISourceRangeList }
    function GetCount: Integer;
    function GetRange(const AIndex: Integer): TSourceRange;
    function GetHandle: PCXSourceRangeList;
  public
    constructor Create(const AHandle: PCXSourceRangeList);
  {$ENDREGION 'Internal Declarations'}
  public
    destructor Destroy; override;
  end;

type
  TEvalResult = class(TInterfacedObject, IEvalResult)
  {$REGION 'Internal Declarations'}
  private
    FHandle: TCXEvalResult;
  protected
    { IEvalResult }
    function GetKind: TEvalResultKind;
    function GetAsInt: Integer;
    function GetAsInt64: Int64;
    function GetIsUnsignedInt: Boolean;
    function GetAsUnsigned: UInt64;
    function GetAsDouble: Double;
    function GetAsString: String;
    function GetHandle: TCXEvalResult;
  public
    constructor Create(const AHandle: TCXEvalResult);
  {$ENDREGION 'Internal Declarations'}
  public
    destructor Destroy; override;
  end;

type
  TPrintingPolicy = class(TInterfacedObject, IPrintingPolicy)
  {$REGION 'Internal Declarations'}
  private
    FHandle: TCXPrintingPolicy;
  protected
    { IPrintingPolicy }
    function GetProperty(const AProp: TPrintingPolicyProperty): Integer;
    procedure SetProperty(const AProp: TPrintingPolicyProperty; const AValue: Integer);
    function GetHandle: TCXPrintingPolicy;
  public
    constructor Create(const AHandle: TCXPrintingPolicy);
  {$ENDREGION 'Internal Declarations'}
  public
    destructor Destroy; override;
  end;

{ Globals }

function GetClangVersion: String; inline;
begin
  Result := CXStringToString(clang_getClangVersion);
end;

procedure ToggleCrashRecovery(const AEnable: Boolean); inline;
begin
  clang_toggleCrashRecovery(Ord(AEnable));
end;

function GetBuildSessionTimestamp: UInt64; inline;
begin
  Result := clang_getBuildSessionTimestamp;
end;

function GetDefaultCodeCompleteOptions: TCodeCompleteFlags; inline;
begin
  Byte(Result) := clang_defaultCodeCompleteOptions;
end;

function GetDefaultDiagnosticDisplayOptions: TDiagnosticDisplayOptions; inline;
begin
  Byte(Result) := clang_defaultDiagnosticDisplayOptions;
end;

function ConstructUsrForObjCClass(const AClassName: String): TUnifiedSymbolResolution; inline;
begin
  Result.FHandle := clang_constructUSR_ObjCClass(PAnsiChar(AnsiString(AClassName)));
end;

function ConstructUsrForObjCCategory(const AClassName, ACategoryName: String): TUnifiedSymbolResolution; inline;
begin
  Result.FHandle := clang_constructUSR_ObjCCategory(PAnsiChar(AnsiString(AClassName)),
    PAnsiChar(AnsiString(ACategoryName)));
end;

function ConstructUsrForObjCProtocol(const AProtocolName: String): TUnifiedSymbolResolution; inline;
begin
  Result.FHandle := clang_constructUSR_ObjCProtocol(PAnsiChar(AnsiString(AProtocolName)));
end;

function ConstructUsrForObjCIVar(const AName: String;
  const AClassUsr: TUnifiedSymbolResolution): TUnifiedSymbolResolution; inline;
begin
  Result.FHandle := clang_constructUSR_ObjCIvar(PAnsiChar(AnsiString(AName)),
    AClassUsr.FHandle);
end;

function ConstructUsrForObjCMethod(const AName: String;
  const AIsInstanceMethod: Boolean;
  const AClassUsr: TUnifiedSymbolResolution): TUnifiedSymbolResolution; inline;
begin
  Result.FHandle := clang_constructUSR_ObjCMethod(PAnsiChar(AnsiString(AName)),
    Ord(AIsInstanceMethod), AClassUsr.FHandle);
end;

function ConstructUsrForObjCProperty(const AProperty: String;
  const AClassUsr: TUnifiedSymbolResolution): TUnifiedSymbolResolution; inline;
begin
  Result.FHandle := clang_constructUSR_ObjCProperty(PAnsiChar(AnsiString(AProperty)),
    AClassUsr.FHandle);
end;

function CXStringToString(const ASource: TCXString; const ADispose: Boolean = True): String;
begin
  Result := String(AnsiString(clang_getCString(ASource)));

  if (ADispose) then
    clang_disposeString(ASource);
end;

function CursorAndRangeVisitor(AContext: Pointer; ACursor: TCXCursor;
  ARange: TCXSourceRange): TCXVisitorResult; cdecl;
var
  Visitor: PCursorAndRangeVisitorProc absolute AContext;
begin
  if Assigned(Visitor.Anon) then
    Result := Ord(Visitor.Anon(TCursor(ACursor), TSourceRange(ARange)))
  else
  begin
    Assert(Assigned(Visitor.Meth));
    Result := Ord(Visitor.Meth(TCursor(ACursor), TSourceRange(ARange)))
  end;
end;

{ TFileUniqueId }

class operator TFileUniqueId.Equal(const ALeft, ARight: TFileUniqueId): Boolean;
begin
  Result := CompareMem(@ALeft, @ARight, SizeOf(TFileUniqueId));
end;

function TFileUniqueId.IsValid: Boolean;
var
  I: Integer;
begin
  {$IF SizeOf(TCXFileUniqueID) <> 24}
    {$MESSAGE Error 'This code depends on the layout of TCXFileUniqueID'}
  {$ENDIF}

  for I := 0 to Length(FHandle.data) - 1 do
  begin
    if (FHandle.data[I] <> 0) then
      Exit(True);
  end;

  Result := False;
end;

class operator TFileUniqueId.NotEqual(const ALeft,
  ARight: TFileUniqueId): Boolean;
begin
  Result := not CompareMem(@ALeft, @ARight, SizeOf(TFileUniqueId));
end;

{ TFile }

class operator TFile.Equal(const ALeft, ARight: TFile): Boolean;
begin
  Result := (clang_File_isEqual(ALeft.FHandle, ARight.FHandle) <> 0);
end;

function TFile.GetFilename: String;
begin
  Result := CXStringToString(clang_getFileName(FHandle));
end;

function TFile.GetFileTime: TDateTime;
begin
  Result := UnixToDateTime(clang_getFileTime(FHandle));
end;

function TFile.GetRealPathName: String;
begin
  Result := CXStringToString(clang_File_tryGetRealPathName(FHandle));
end;

function TFile.GetUniqueId: TFileUniqueId;
begin
  if (clang_getFileUniqueID(FHandle, @Result.FHandle) <> 0) then
    FillChar(Result, SizeOf(Result), 0);
end;

function TFile.IsNull: Boolean;
begin
  Result := (FHandle = nil);
end;

class operator TFile.NotEqual(const ALeft, ARight: TFile): Boolean;
begin
  Result := (clang_File_isEqual(ALeft.FHandle, ARight.FHandle) = 0);
end;

{ TSourceLocation }

class operator TSourceLocation.Equal(const ALeft,
  ARight: TSourceLocation): Boolean;
begin
  Result := (clang_equalLocations(ALeft.FHandle, ARight.FHandle) <> 0);
end;

procedure TSourceLocation.GetExpansionLocation(out AFile: TFile; out ALine,
  AColumn, AOffset: Integer);
begin
  clang_getExpansionLocation(FHandle, @AFile, @ALine, @AColumn, @AOffset);
end;

procedure TSourceLocation.GetFileLocation(out AFile: TFile; out ALine, AColumn,
  AOffset: Integer);
begin
  clang_getFileLocation(FHandle, @AFile, @ALine, @AColumn, @AOffset);
end;

function TSourceLocation.GetIsFromMainFile: Boolean;
begin
  Result := (clang_Location_isFromMainFile(FHandle) <> 0);
end;

function TSourceLocation.GetIsInSystemHeader: Boolean;
begin
  Result := (clang_Location_isInSystemHeader(FHandle) <> 0);
end;

procedure TSourceLocation.GetPresumedLocation(out AFilename: String; out ALine,
  AColumn: Integer);
var
  S: TCXString;
begin
  clang_getPresumedLocation(FHandle, @S, @ALine, @AColumn);
  AFilename := CXStringToString(S);
end;

procedure TSourceLocation.GetSpellingLocation(out AFile: TFile; out ALine,
  AColumn, AOffset: Integer);
begin
  clang_getSpellingLocation(FHandle, @AFile, @ALine, @AColumn, @AOffset);
end;

class operator TSourceLocation.NotEqual(const ALeft,
  ARight: TSourceLocation): Boolean;
begin
  Result := (clang_equalLocations(ALeft.FHandle, ARight.FHandle) = 0);
end;

class function TSourceLocation.Null: TSourceLocation;
begin
  Result.FHandle := clang_getNullLocation;
end;

{ TSourceRange }

constructor TSourceRange.Create(const ABegin, AEnd: TSourceLocation);
begin
  FHandle := clang_getRange(ABegin.FHandle, AEnd.FHandle);
end;

class operator TSourceRange.Equal(const ALeft, ARight: TSourceRange): Boolean;
begin
  Result := (clang_equalRanges(ALeft.FHandle, ARight.FHandle) <> 0);
end;

function TSourceRange.GetFirst: TSourceLocation;
begin
  Result.FHandle := clang_getRangeStart(FHandle);
end;

function TSourceRange.GetLast: TSourceLocation;
begin
  Result.FHandle := clang_getRangeEnd(FHandle);
end;

function TSourceRange.IsNull: Boolean;
begin
  Result := (clang_Range_isNull(FHandle) <> 0);
end;

class operator TSourceRange.NotEqual(const ALeft,
  ARight: TSourceRange): Boolean;
begin
  Result := (clang_equalRanges(ALeft.FHandle, ARight.FHandle) = 0);
end;

class function TSourceRange.Null: TSourceRange;
begin
  Result.FHandle := clang_getNullRange;
end;

{ TModule }

function TModule.GetAstFile: TFile;
begin
  Result.FHandle := clang_Module_getASTFile(FHandle);
end;

function TModule.GetFullName: String;
begin
  Result := CXStringToString(clang_Module_getFullName(FHandle));
end;

function TModule.GetIsSystem: Boolean;
begin
  Result := (clang_Module_isSystem(FHandle) <> 0);
end;

function TModule.GetName: String;
begin
  Result := CXStringToString(clang_Module_getName(FHandle));
end;

function TModule.GetParent: TModule;
begin
  Result.FHandle := clang_Module_getParent(FHandle);
end;

function TModule.IsNull: Boolean;
begin
  Result := (FHandle = nil);
end;

{ TIndex }

constructor TIndex.Create(const AExcludeDeclarationsFromPCH,
  ADisplayDiagnostics: Boolean);
begin
  inherited Create;
  FHandle := clang_createIndex(Ord(AExcludeDeclarationsFromPCH),
    Ord(ADisplayDiagnostics));
end;

function TIndex.CreateIndexAction: IIndexAction;
var
  Action: TCXIndexAction;
begin
  Action := clang_IndexAction_create(FHandle);
  if (Action = nil) then
    Result := nil
  else
    Result := TIndexAction.Create(Action);
end;

function TIndex.CreateTranslationUnit(
  const AAstFilename: String): ITranslationUnit;
var
  TU: TCXTranslationUnit;
begin
  TU := clang_createTranslationUnit(FHandle, PAnsiChar(AnsiString(AAstFilename)));
  if (TU = nil) then
    Result := nil
  else
    Result := TTranslationUnit.Create(TU);
end;

function TIndex.CreateTranslationUnitFromSource(const ASourceFilename: String;
  const AUnsavedFiles: TArray<TUnsavedFile>): ITranslationUnit;
begin
  Result := DoCreateTranslationUnit(ASourceFilename, [], AUnsavedFiles, [], False);
end;

function TIndex.CreateTranslationUnitFromSource(const ASourceFilename: String;
  const AClangCommandLineArgs: array of String;
  const AUnsavedFiles: TArray<TUnsavedFile>): ITranslationUnit;
begin
  Result := DoCreateTranslationUnit(ASourceFilename, AClangCommandLineArgs,
    AUnsavedFiles, [], False);
end;

destructor TIndex.Destroy;
begin
  clang_disposeIndex(FHandle);
  inherited;
end;

function TIndex.DoCreateTranslationUnit(const ASourceFilename: String;
  const AClangCommandLineArgs: array of String;
  const AUnsavedFiles: TArray<TUnsavedFile>;
  const AOptions: TTranslationUnitFlags;
  const AParse: Boolean): ITranslationUnit;
var
  SrcFilename: AnsiString;
  SrcFilenamePtr: PAnsiChar;
  Args: TArray<AnsiString>;
  ArgPtrs: TArray<PAnsiChar>;
  ArgsPtr: PPAnsiChar;
  Files: TArray<TCXUnsavedFile>;
  Filenames: TArray<AnsiString>;
  FilesPtr: PCXUnsavedFile;
  TU: TCXTranslationUnit;
  I, NumArgs, NumFiles: Integer;
begin
  if (ASourceFilename = '') then
    SrcFilenamePtr := nil
  else
  begin
    SrcFilename := AnsiString(ASourceFilename);
    SrcFilenamePtr := Pointer(SrcFilename);
  end;

  NumArgs := Length(AClangCommandLineArgs);
  if (NumArgs = 0) then
    ArgsPtr := nil
  else
  begin
    SetLength(Args, NumArgs);
    SetLength(ArgPtrs, NumArgs);
    for I := 0 to NumArgs - 1 do
    begin
      Args[I] := AnsiString(AClangCommandLineArgs[I]);
      ArgPtrs[I] := Pointer(Args[I]);
    end;
    ArgsPtr := @ArgPtrs[0];
  end;

  NumFiles := Length(AUnsavedFiles);
  if (NumFiles = 0) then
    FilesPtr := nil
  else
  begin
    SetLength(Files, NumFiles);
    SetLength(Filenames, NumFiles);
    for I := 0 to NumFiles - 1 do
    begin
      FileNames[I] := AnsiString(AUnsavedFiles[I].Filename);
      Files[I].Filename := Pointer(FileNames[I]);
      Files[I].Contents := Pointer(AUnsavedFiles[I].Contents);
      Files[I].Length := Length(AUnsavedFiles[I].Contents);
    end;
    FilesPtr := @Files[0];
  end;

  if (AParse) then
    TU := clang_parseTranslationUnit(FHandle, SrcFilenamePtr, ArgsPtr, NumArgs,
      FilesPtr, NumFiles, Word(AOptions))
  else
    TU := clang_createTranslationUnitFromSourceFile(FHandle, SrcFilenamePtr,
      NumArgs, ArgsPtr, NumFiles, FilesPtr);

  if (TU = nil) then
    Result := nil
  else
    Result := TTranslationUnit.Create(TU);
end;

function TIndex.GetGlobalOptions: TGlobalOptions;
begin
  Byte(Result) := clang_CXIndex_getGlobalOptions(FHandle);
end;

function TIndex.GetHandle: TCXIndex;
begin
  Result := FHandle;
end;

function TIndex.ParseTranslationUnit(const ASourceFilename: String;
  const AClangCommandLineArgs: array of String;
  const AUnsavedFiles: TArray<TUnsavedFile>;
  const AOptions: TTranslationUnitFlags): ITranslationUnit;
begin
  Result := DoCreateTranslationUnit(ASourceFilename, AClangCommandLineArgs,
    AUnsavedFiles, AOptions, True);
end;

procedure TIndex.SetGlobalOptions(const AValue: TGlobalOptions);
begin
  clang_CXIndex_setGlobalOptions(FHandle, Byte(AValue));
end;

procedure TIndex.SetInvocationEmissionPath(const AValue: String);
begin
  if (AValue = '') then
    clang_CXIndex_setInvocationEmissionPathOption(FHandle, nil)
  else
    clang_CXIndex_setInvocationEmissionPathOption(FHandle, PAnsiChar(AnsiString(AValue)));
end;

{ TTranslationUnit }

function TTranslationUnit.AnnotateTokens(
  const ATokens: TArray<TToken>): TArray<TCursor>;
begin
  SetLength(Result, Length(ATokens));
  clang_annotateTokens(FHandle, @ATokens[0], Length(ATokens), @Result[0]);
end;

function TTranslationUnit.AnnotateTokens(
  const ATokens: ITokenList): TArray<TCursor>;
var
  Tokens: TTokenList;
begin
  Tokens := ATokens as TTokenList;
  SetLength(Result, Tokens.FCount);
  clang_annotateTokens(FHandle, Tokens.FTokens, Tokens.FCount, @Result[0]);
end;

function TTranslationUnit.CodeCompleteAt(const ACompleteFilename: String;
  const ACompleteLine, ACompleteColumn: Integer;
  const AUnsavedFiles: TArray<TUnsavedFile>;
  const AOptions: TCodeCompleteFlags): ICodeCompleteResults;
var
  CCR: PCXCodeCompleteResults;
  Files: TArray<TCXUnsavedFile>;
  Filenames: TArray<AnsiString>;
  FilesPtr: PCXUnsavedFile;
  I, NumFiles: Integer;
begin
  NumFiles := Length(AUnsavedFiles);
  if (NumFiles = 0) then
    FilesPtr := nil
  else
  begin
    SetLength(Files, NumFiles);
    SetLength(Filenames, NumFiles);
    for I := 0 to NumFiles - 1 do
    begin
      FileNames[I] := AnsiString(AUnsavedFiles[I].Filename);
      Files[I].Filename := Pointer(FileNames[I]);
      Files[I].Contents := Pointer(AUnsavedFiles[I].Contents);
      Files[I].Length := Length(AUnsavedFiles[I].Contents);
    end;
    FilesPtr := @Files[0];
  end;

  CCR := clang_codeCompleteAt(FHandle, PAnsiChar(AnsiString(ACompleteFilename)),
    ACompleteLine, ACompleteColumn, FilesPtr, NumFiles, Byte(AOptions));

  if (CCR = nil) then
    Result := nil
  else
    Result := TCodeCompleteResults.Create(CCR);
end;

constructor TTranslationUnit.Create(const AHandle: TCXTranslationUnit);
begin
  inherited Create;
  FHandle := AHandle;
end;

destructor TTranslationUnit.Destroy;
begin
  clang_disposeTranslationUnit(FHandle);
  inherited;
end;

function TTranslationUnit.FindIncludesInFile(const AFile: TFile;
  const AVisitor: TCursorAndRangeVisitorMethod): TVisitResult;
var
  VisitorProc: TCursorAndRangeVisitorProc;
  Visitor: TCXCursorAndRangeVisitor;
begin
  VisitorProc.Anon := nil;
  VisitorProc.Meth := AVisitor;

  Visitor.context := @VisitorProc;
  Visitor.visit := CursorAndRangeVisitor;

  Result := TVisitResult(clang_findIncludesInFile(FHandle, AFile.FHandle, Visitor));
end;

function TTranslationUnit.FindIncludesInFile(const AFile: TFile;
  const AVisitor: TCursorAndRangeVisitor): TVisitResult;
var
  VisitorProc: TCursorAndRangeVisitorProc;
  Visitor: TCXCursorAndRangeVisitor;
begin
  VisitorProc.Anon := AVisitor;
  VisitorProc.Meth := nil;

  Visitor.context := @VisitorProc;
  Visitor.visit := CursorAndRangeVisitor;

  Result := TVisitResult(clang_findIncludesInFile(FHandle, AFile.FHandle, Visitor));
end;

function TTranslationUnit.GetAllDiagnostics: IDiagnosticSet;
var
  DS: TCXDiagnosticSet;
begin
  DS := clang_getDiagnosticSetFromTU(FHandle);
  if (DS = nil) then
    Result := nil
  else
    Result := TDiagnosticSet.Create(DS);
end;

function TTranslationUnit.GetAllSkippedRanges: ISourceRangeList;
var
  Ranges: PCXSourceRangeList;
begin
  Ranges := clang_getAllSkippedRanges(FHandle);
  if (Ranges = nil) then
    Result := nil
  else
    Result := TSourceRangeList.Create(Ranges);
end;

function TTranslationUnit.GetCursor(
  const ASourceLocation: TSourceLocation): TCursor;
begin
  Result.FHandle := clang_getCursor(FHandle, ASourceLocation.FHandle);
end;

function TTranslationUnit.GetCursor: TCursor;
begin
  Result.FHandle := clang_getTranslationUnitCursor(FHandle);
end;

function TTranslationUnit.GetDiagnostic(const AIndex: Integer): IDiagnostic;
var
  D: TCXDiagnostic;
begin
  D := clang_getDiagnostic(FHandle, AIndex);
  if (D = nil) then
    Result := nil
  else
    Result := TDiagnostic.Create(D);
end;

function TTranslationUnit.GetDiagnosticCount: Integer;
begin
  Result := clang_getNumDiagnostics(FHandle);
end;

function TTranslationUnit.GetFile(const AFilename: String): TFile;
begin
  Result.FHandle := clang_getFile(FHandle, PAnsiChar(AnsiString(AFilename)));
end;

function TTranslationUnit.GetFileContents(const AFile: TFile): TBytes;
var
  Size: SIZE_T;
  Buf: Pointer;
begin
  Buf := clang_getFileContents(FHandle, AFile.FHandle, @Size);
  SetLength(Result, Size);
  Move(Buf^, Result[0], Size);
end;

function TTranslationUnit.GetHandle: TCXTranslationUnit;
begin
  Result := FHandle;
end;

procedure TTranslationUnit.GetInclusions(
  const AVisitor: TInclusionVisitorMethod);
var
  Proc: TInclusionVisitorProc;
begin
  Proc.Anon := nil;
  Proc.Meth := AVisitor;
  clang_getInclusions(FHandle, InclusionVisitor, @Proc);
end;

procedure TTranslationUnit.GetInclusions(const AVisitor: TInclusionVisitor);
var
  Proc: TInclusionVisitorProc;
begin
  Proc.Anon := AVisitor;
  Proc.Meth := nil;
  clang_getInclusions(FHandle, InclusionVisitor, @Proc);
end;

function TTranslationUnit.GetLocation(const AFile: TFile;
  const AOffset: Integer): TSourceLocation;
begin
  Result.FHandle := clang_getLocationForOffset(FHandle, AFile.FHandle, AOffset);
end;

function TTranslationUnit.GetModule(const AFile: TFile): TModule;
begin
  Result.FHandle := clang_getModuleForFile(FHandle, AFile.FHandle);
end;

function TTranslationUnit.GetResourceUsage: IResourceUsage;
var
  RU: TCXTUResourceUsage;
begin
  RU := clang_getCXTUResourceUsage(FHandle);
  Result := TResourceUsage.Create(RU);
end;

function TTranslationUnit.GetSkippedRanges(
  const AFile: TFile): ISourceRangeList;
var
  Ranges: PCXSourceRangeList;
begin
  Ranges := clang_getSkippedRanges(FHandle, AFile.FHandle);
  if (Ranges = nil) then
    Result := nil
  else
    Result := TSourceRangeList.Create(Ranges);
end;

function TTranslationUnit.GetSpelling: String;
begin
  Result := CXStringToString(clang_getTranslationUnitSpelling(FHandle));
end;

function TTranslationUnit.GetTargetInfo: ITargetInfo;
var
  TI: TCXTargetInfo;
begin
  TI := clang_getTranslationUnitTargetInfo(FHandle);
  if (TI = nil) then
    Result := nil
  else
    Result := TTargetInfo.Create(TI);
end;

function TTranslationUnit.GetToken(
  const ALocation: TSourceLocation): ITokenList;
var
  Token: PCXToken;
begin
  Token := clang_getToken(FHandle, ALocation.FHandle);
  if (Token = nil) then
    Result := nil
  else
    Result := TTokenList.Create(Self, Token, 1);
end;

function TTranslationUnit.GetTokenExtent(const AToken: TToken): TSourceRange;
begin
  Result.FHandle := clang_getTokenExtent(FHandle, AToken.FHandle);
end;

function TTranslationUnit.GetTokenLocation(
  const AToken: TToken): TSourceLocation;
begin
  Result.FHandle := clang_getTokenLocation(FHandle, AToken.FHandle);
end;

function TTranslationUnit.GetTokenSpelling(const AToken: TToken): String;
begin
  Result := CXStringToString(clang_getTokenSpelling(FHandle, AToken.FHandle));
end;

function TTranslationUnit.GetTopLevelHeader(const AModule: TModule;
  const AIndex: Integer): TFile;
begin
  Result.FHandle := clang_Module_getTopLevelHeader(FHandle, AModule.FHandle, AIndex);
end;

function TTranslationUnit.GetTopLevelHeaderCount(
  const AModule: TModule): Integer;
begin
  Result := clang_Module_getNumTopLevelHeaders(FHandle, AModule.FHandle);
end;

function TTranslationUnit.GetLocation(const AFile: TFile; const ALine,
  AColumn: Integer): TSourceLocation;
begin
  Result.FHandle := clang_getLocation(FHandle, AFile.FHandle, ALine, AColumn);
end;

class procedure TTranslationUnit.InclusionVisitor(AIncludedFile: TCXFile;
  AInclusionStack: PCXSourceLocation; AIncludeLen: Cardinal;
  AClientData: TCXClientData);
var
  Visitor: PInclusionVisitorProc absolute AClientData;
begin
  if Assigned(Visitor.Anon) then
    Visitor.Anon(TFile(AIncludedFile), PSourceLocation(AInclusionStack), AIncludeLen)
  else
  begin
    Assert(Assigned(Visitor.Meth));
    Visitor.Meth(TFile(AIncludedFile), PSourceLocation(AInclusionStack), AIncludeLen)
  end;
end;

function TTranslationUnit.IsFileMultipleIncludeGuarded(
  const AFile: TFile): Boolean;
begin
  Result := (clang_isFileMultipleIncludeGuarded(FHandle, AFile.FHandle) <> 0);
end;

function TTranslationUnit.Reparse(
  const AUnsavedFiles: TArray<TUnsavedFile>): TError;
var
  Files: TArray<TCXUnsavedFile>;
  Filenames: TArray<AnsiString>;
  FilesPtr: PCXUnsavedFile;
  I, NumFiles: Integer;
begin
  NumFiles := Length(AUnsavedFiles);
  if (NumFiles = 0) then
    FilesPtr := nil
  else
  begin
    SetLength(Files, NumFiles);
    SetLength(Filenames, NumFiles);
    for I := 0 to NumFiles - 1 do
    begin
      FileNames[I] := AnsiString(AUnsavedFiles[I].Filename);
      Files[I].Filename := Pointer(FileNames[I]);
      Files[I].Contents := Pointer(AUnsavedFiles[I].Contents);
      Files[I].Length := Length(AUnsavedFiles[I].Contents);
    end;
    FilesPtr := @Files[0];
  end;

  Result := TError(clang_reparseTranslationUnit(FHandle, NumFiles, FilesPtr, 0));
end;

function TTranslationUnit.Save(const AFilename: String): TSaveResult;
begin
  Result := TSaveResult(clang_saveTranslationUnit(FHandle,
    PAnsiChar(AnsiString(AFilename)), 0));
end;

procedure TTranslationUnit.Suspend;
begin
  clang_suspendTranslationUnit(FHandle);
end;

function TTranslationUnit.Tokenize(const ARange: TSourceRange): ITokenList;
var
  Tokens: PCXToken;
  NumTokens: Cardinal;
begin
  clang_tokenize(FHandle, ARange.FHandle, @Tokens, @NumTokens);
  Result := TTokenList.Create(Self, Tokens, NumTokens);
end;

{ TIndexAction }

class function TIndexAction.CallbackAbortQuery(AClientData: TCXClientData;
  AReserved: Pointer): Integer;
var
  Listener: IIndexerListener;
begin
  Listener := IIndexerListener(AClientData);
  Result := Ord(Listener.AbortQuery(AReserved));
end;

class procedure TIndexAction.CallbackDiagnostic(AClientData: TCXClientData;
  ADiagnostics: TCXDiagnosticSet; AReserved: Pointer);
var
  Listener: IIndexerListener;
  Diagnostics: IDiagnosticSet;
begin
  Listener := IIndexerListener(AClientData);
  Diagnostics := TDiagnosticSet.Create(ADiagnostics, False);
  Listener.Diagnostic(Diagnostics, AReserved);
end;

class function TIndexAction.CallbackEnteredMainFile(AClientData: TCXClientData;
  AMainFile: TCXFile; AReserved: Pointer): TCXIdxClientFile;
var
  Listener: IIndexerListener;
  F: TFile;
begin
  Listener := IIndexerListener(AClientData);
  F.FHandle := AMainFile;
  Result := Listener.EnteredMainFile(F, AReserved);
end;

class function TIndexAction.CallbackImportedAstFile(AClientData: TCXClientData;
  const AInfo: PCXIdxImportedASTFileInfo): TCXIdxClientASTFile;
var
  Listener: IIndexerListener;
  Info: TIdxImportedAstFileInfo;
begin
  Listener := IIndexerListener(AClientData);
  Info.FHandle := AInfo;
  Result := Listener.ImportedAstFile(Info);
end;

class function TIndexAction.CallbackIncludedFile(AClientData: TCXClientData;
  const AInfo: PCXIdxIncludedFileInfo): TCXIdxClientFile;
var
  Listener: IIndexerListener;
  Info: TIdxIncludedFileInfo;
begin
  Listener := IIndexerListener(AClientData);
  Info.FHandle := AInfo;
  Result := Listener.IncludedFile(Info);
end;

class procedure TIndexAction.CallbackIndexDeclaration(
  AClientData: TCXClientData; const AInfo: PCXIdxDeclInfo);
var
  Listener: IIndexerListener;
  Info: TIdxDeclInfo;
begin
  Listener := IIndexerListener(AClientData);
  Info.FHandle := AInfo;
  Listener.IndexDeclaration(Info);
end;

class procedure TIndexAction.CallbackIndexEntityReference(
  AClientData: TCXClientData; const AInfo: PCXIdxEntityRefInfo);
var
  Listener: IIndexerListener;
  Info: TIdxEntityRefInfo;
begin
  Listener := IIndexerListener(AClientData);
  Info.FHandle := AInfo;
  Listener.IndexEntityReference(Info);
end;

class function TIndexAction.CallbackStartedTranslationUnit(
  AClientData: TCXClientData; AReserved: Pointer): TCXIdxClientContainer;
var
  Listener: IIndexerListener;
begin
  Listener := IIndexerListener(AClientData);
  Result := Listener.StartedTranslationUnit(AReserved);
end;

constructor TIndexAction.Create(const AHandle: TCXIndexAction);
begin
  inherited Create;
  FHandle := AHandle;
end;

destructor TIndexAction.Destroy;
begin
  clang_IndexAction_dispose(FHandle);
  inherited;
end;

function TIndexAction.GetHandle: TCXIndexAction;
begin
  Result := FHandle;
end;

function TIndexAction.IndexSourceFile(const AListener: IIndexerListener;
  const AOptions: TIndexOptions; const ASourceFilename: String;
  const AClangCommandLineArgs: array of String;
  const AUnsavedFiles: TArray<TUnsavedFile>;
  const ATUOptions: TTranslationUnitFlags;
  out ATranslationUnit: ITranslationUnit): TError;
var
  SrcFilename: AnsiString;
  SrcFilenamePtr: PAnsiChar;
  Args: TArray<AnsiString>;
  ArgPtrs: TArray<PAnsiChar>;
  ArgsPtr: PPAnsiChar;
  Files: TArray<TCXUnsavedFile>;
  Filenames: TArray<AnsiString>;
  FilesPtr: PCXUnsavedFile;
  Callbacks: TIndexerCallbacks;
  TU: TCXTranslationUnit;
  I, NumArgs, NumFiles: Integer;
begin
  if (ASourceFilename = '') then
    SrcFilenamePtr := nil
  else
  begin
    SrcFilename := AnsiString(ASourceFilename);
    SrcFilenamePtr := Pointer(SrcFilename);
  end;

  NumArgs := Length(AClangCommandLineArgs);
  if (NumArgs = 0) then
    ArgsPtr := nil
  else
  begin
    SetLength(Args, NumArgs);
    SetLength(ArgPtrs, NumArgs);
    for I := 0 to NumArgs - 1 do
    begin
      Args[I] := AnsiString(AClangCommandLineArgs[I]);
      ArgPtrs[I] := Pointer(Args[I]);
    end;
    ArgsPtr := @ArgPtrs[0];
  end;

  NumFiles := Length(AUnsavedFiles);
  if (NumFiles = 0) then
    FilesPtr := nil
  else
  begin
    SetLength(Files, NumFiles);
    SetLength(Filenames, NumFiles);
    for I := 0 to NumFiles - 1 do
    begin
      FileNames[I] := AnsiString(AUnsavedFiles[I].Filename);
      Files[I].Filename := Pointer(FileNames[I]);
      Files[I].Contents := Pointer(AUnsavedFiles[I].Contents);
      Files[I].Length := Length(AUnsavedFiles[I].Contents);
    end;
    FilesPtr := @Files[0];
  end;

  SetupCallbacks(Callbacks);

  Result := TError(clang_indexSourceFile(FHandle, Pointer(AListener),
    @Callbacks, SizeOf(Callbacks), Byte(AOptions), SrcFilenamePtr, ArgsPtr,
    NumArgs, FilesPtr, NumFiles, @TU, Word(ATUOptions)));

  if (TU = nil) then
    ATranslationUnit := nil
  else
    ATranslationUnit := TTranslationUnit.Create(TU);
end;

function TIndexAction.IndexTranslationUnit(const AListener: IIndexerListener;
  const AOptions: TIndexOptions;
  const ATranslationUnit: ITranslationUnit): TError;
var
  TU: TCXTranslationUnit;
  Callbacks: TIndexerCallbacks;
begin
  if Assigned(ATranslationUnit) then
    TU := ATranslationUnit.Handle
  else
    TU := nil;

  SetupCallbacks(Callbacks);

  Result := TError(clang_indexTranslationUnit(FHandle, Pointer(AListener),
    @Callbacks, SizeOf(Callbacks), Byte(AOptions), TU));
end;

class procedure TIndexAction.SetupCallbacks(out ACallbacks: TIndexerCallbacks);
begin
  ACallbacks.abortQuery := CallbackAbortQuery;
  ACallbacks.diagnostic := CallbackDiagnostic;
  ACallbacks.enteredMainFile := CallbackEnteredMainFile;
  ACallbacks.ppIncludedFile := CallbackIncludedFile;
  ACallbacks.importedASTFile := CallbackImportedAstFile;
  ACallbacks.startedTranslationUnit := CallbackStartedTranslationUnit;
  ACallbacks.indexDeclaration := CallbackIndexDeclaration;
  ACallbacks.indexEntityReference := CallbackIndexEntityReference;
end;

function TIndexAction.IndexSourceFile(const AListener: IIndexerListener;
  const AOptions: TIndexOptions; const ASourceFilename: String;
  const AClangCommandLineArgs: array of String;
  const AUnsavedFiles: TArray<TUnsavedFile>;
  const ATUOptions: TTranslationUnitFlags): TError;
var
  TU: ITranslationUnit;
begin
  Result := IndexSourceFile(AListener, AOptions, ASourceFilename,
    AClangCommandLineArgs, AUnsavedFiles, ATUOptions, TU);
end;

{ TDiagnostic }

constructor TDiagnostic.Create(const AHandle: TCXDiagnostic);
begin
  inherited Create;
  FHandle := AHandle;
end;

destructor TDiagnostic.Destroy;
begin
  clang_disposeDiagnostic(FHandle);
  inherited;
end;

function TDiagnostic.FixIt(const AFixItIndex: Integer;
  out AReplacementRange: TSourceRange): String;
begin
  Result := CXStringToString(clang_getDiagnosticFixIt(FHandle, AFixItIndex,
    @AReplacementRange));
end;

function TDiagnostic.Format(const AOptions: TDiagnosticDisplayOptions): String;
begin
  Result := CXStringToString(clang_formatDiagnostic(FHandle, Byte(AOptions)));
end;

function TDiagnostic.GetCategory: Integer;
begin
  Result := clang_getDiagnosticCategory(FHandle);
end;

function TDiagnostic.GetCategoryText: String;
begin
  Result := CXStringToString(clang_getDiagnosticCategoryText(FHandle));
end;

function TDiagnostic.GetChildDiagnostics: IDiagnosticSet;
var
  DS: TCXDiagnosticSet;
begin
  DS := clang_getChildDiagnostics(FHandle);
  if (DS = nil) then
    Result := nil
  else
    Result := TDiagnosticSet.Create(DS, False);
end;

function TDiagnostic.GetDisableOption: String;
var
  E, D: TCXString;
begin
  E := clang_getDiagnosticOption(FHandle, @D);
  clang_disposeString(E);
  Result := CXStringToString(D);
end;

function TDiagnostic.GetEnableOption: String;
begin
  Result := CXStringToString(clang_getDiagnosticOption(FHandle, nil));
end;

function TDiagnostic.GetFixItCount: Integer;
begin
  Result := clang_getDiagnosticNumFixIts(FHandle);
end;

function TDiagnostic.GetHandle: TCXDiagnostic;
begin
  Result := FHandle;
end;

function TDiagnostic.GetLocation: TSourceLocation;
begin
  Result.FHandle := clang_getDiagnosticLocation(FHandle);
end;

function TDiagnostic.GetRange(const AIndex: Integer): TSourceRange;
begin
  Result.FHandle := clang_getDiagnosticRange(FHandle, AIndex);
end;

function TDiagnostic.GetRangeCount: Integer;
begin
  Result := clang_getDiagnosticNumRanges(FHandle);
end;

function TDiagnostic.GetSeverity: TDiagnosticSeverity;
begin
  Result := TDiagnosticSeverity(clang_getDiagnosticSeverity(FHandle));
end;

function TDiagnostic.GetSpelling: String;
begin
  Result := CXStringToString(clang_getDiagnosticSpelling(FHandle));
end;

{ TResourceUsage }

constructor TResourceUsage.Create(const AHandle: TCXTUResourceUsage);
begin
  inherited Create;
  FHandle := AHandle;
end;

destructor TResourceUsage.Destroy;
begin
  clang_disposeCXTUResourceUsage(FHandle);
  inherited;
end;

function TResourceUsage.GetHandle: TCXTUResourceUsage;
begin
  Result := FHandle;
end;

{ TTargetInfo }

constructor TTargetInfo.Create(const AHandle: TCXTargetInfo);
begin
  inherited Create;
  FHandle := AHandle;
end;

destructor TTargetInfo.Destroy;
begin
  clang_TargetInfo_dispose(FHandle);
  inherited;
end;

function TTargetInfo.GetHandle: TCXTargetInfo;
begin
  Result := FHandle;
end;

function TTargetInfo.GetPointerWidth: Integer;
begin
  Result := clang_TargetInfo_getPointerWidth(FHandle);
end;

function TTargetInfo.GetTriple: String;
begin
  Result := CXStringToString(clang_TargetInfo_getTriple(FHandle));
end;

{ TTokenList }

constructor TTokenList.Create(const ATranslationUnit: ITranslationUnit;
  const ATokens: PCXToken; const ACount: Integer);
begin
  inherited Create;
  { Translation unit must be kept alive for the duration of this list. }
  FTranslationUnit := ATranslationUnit;
  FTokens := ATokens;
  FCount := ACount;
end;

destructor TTokenList.Destroy;
begin
  clang_disposeTokens(FTranslationUnit.Handle, FTokens, FCount);
  inherited;
end;

function TTokenList.GetCount: Integer;
begin
  Result := FCount;
end;

function TTokenList.GetToken(const AIndex: Integer): TToken;
begin
  Result.FHandle := FTokens[AIndex];
end;

{ TCodeCompleteResults }

constructor TCodeCompleteResults.Create(const AHandle: PCXCodeCompleteResults);
begin
  inherited Create;
  FHandle := AHandle;
end;

destructor TCodeCompleteResults.Destroy;
begin
  clang_disposeCodeCompleteResults(FHandle);
  inherited;
end;

function TCodeCompleteResults.GetCompletionFixIt(const AIndex,
  AFixItIndex: Integer; const AReplacementRange: TSourceRange): String;
begin
  Result := CXStringToString(clang_getCompletionFixIt(FHandle, AIndex,
    AFixItIndex, @AReplacementRange.FHandle));
end;

function TCodeCompleteResults.GetContainerComplete: Boolean;
var
  C: Cardinal;
begin
  clang_codeCompleteGetContainerKind(FHandle, @C);
  Result := (C = 0);
end;

function TCodeCompleteResults.GetContainerKind: TCursorKind;
var
  C: Cardinal;
begin
  Result := TCursorKind(clang_codeCompleteGetContainerKind(FHandle, @C));
end;

function TCodeCompleteResults.GetContainerUsr: TUnifiedSymbolResolution;
begin
  Result.FHandle := clang_codeCompleteGetContainerUSR(FHandle);
end;

function TCodeCompleteResults.GetContexts: UInt64;
begin
  Result := clang_codeCompleteGetContexts(FHandle);
end;

function TCodeCompleteResults.GetCount: Integer;
begin
  Result := FHandle.NumResults;
end;

function TCodeCompleteResults.GetDiagnostic(const AIndex: Integer): IDiagnostic;
var
  D: TCXDiagnostic;
begin
  D := clang_codeCompleteGetDiagnostic(FHandle, AIndex);
  if (D = nil) then
    Result := nil
  else
    Result := TDiagnostic.Create(D);
end;

function TCodeCompleteResults.GetDiagnosticCount: Integer;
begin
  Result := clang_codeCompleteGetNumDiagnostics(FHandle);
end;

function TCodeCompleteResults.GetFixItCount(const AIndex: Integer): Integer;
begin
  Result := clang_getCompletionNumFixIts(FHandle, AIndex);
end;

function TCodeCompleteResults.GetHandle: PCXCodeCompleteResults;
begin
  Result := FHandle;
end;

function TCodeCompleteResults.GetObjCSelector: String;
begin
  Result := CXStringToString(clang_codeCompleteGetObjCSelector(FHandle));
end;

function TCodeCompleteResults.GetResult(
  const AIndex: Integer): TCompletionResult;
begin
  Result.FHandle := FHandle.Results[AIndex];
end;

{ TCursor }

class operator TCursor.Equal(const ALeft, ARight: TCursor): Boolean;
begin
  Result := (clang_equalCursors(ALeft.FHandle, ARight.FHandle) <> 0);
end;

function TCursor.Evaluate: IEvalResult;
var
  ER: TCXEvalResult;
begin
  ER := clang_Cursor_Evaluate(FHandle);
  if (ER = nil) then
    Result := nil
  else
    Result := TEvalResult.Create(ER);
end;

function TCursor.GetArgument(const AIndex: Integer): TCursor;
begin
  Result.FHandle := clang_Cursor_getArgument(FHandle, AIndex);
end;

function TCursor.GetArgumentCount: Integer;
begin
  Result := clang_Cursor_getNumArguments(FHandle);
end;

function TCursor.GetAvailability: TAvailabilityKind;
begin
  Result := TAvailabilityKind(clang_getCursorAvailability(FHandle));
end;

function TCursor.GetBriefComment: String;
begin
  Result := CXStringToString(clang_Cursor_getBriefCommentText(FHandle));
end;

function TCursor.GetCanonical: TCursor;
begin
  Result.FHandle := clang_getCanonicalCursor(FHandle);
end;

function TCursor.GetCommentRange: TSourceRange;
begin
  Result.FHandle := clang_Cursor_getCommentRange(FHandle);
end;

function TCursor.GetCompletionString: TCompletionString;
begin
  Result.FHandle := clang_getCursorCompletionString(FHandle);
end;

function TCursor.GetCursorType: TType;
begin
  Result.FHandle := clang_getCursorType(FHandle);
end;

function TCursor.GetCxxAccessSpecifier: TCxxAccessSpecifier;
begin
  Result := TCxxAccessSpecifier(clang_getCXXAccessSpecifier(FHandle));
end;

function TCursor.GetCxxConstructorIsConvertingConstructor: Boolean;
begin
  Result := (clang_CXXConstructor_isConvertingConstructor(FHandle) <> 0);
end;

function TCursor.GetCxxConstructorIsCopyConstructor: Boolean;
begin
  Result := (clang_CXXConstructor_isCopyConstructor(FHandle) <> 0);
end;

function TCursor.GetCxxConstructorIsDefaultConstructor: Boolean;
begin
  Result := (clang_CXXConstructor_isDefaultConstructor(FHandle) <> 0);
end;

function TCursor.GetCxxConstructorIsMoveConstructor: Boolean;
begin
  Result := (clang_CXXConstructor_isMoveConstructor(FHandle) <> 0);
end;

function TCursor.GetCxxFieldIsMutable: Boolean;
begin
  Result := (clang_CXXField_isMutable(FHandle) <> 0);
end;

function TCursor.GetCxxManglings: TArray<String>;
var
  Strings: PCXStringSet;
  I: Integer;
begin
  Strings := clang_Cursor_getCXXManglings(FHandle);
  try
    SetLength(Result, Strings.Count);
    for I := 0 to Strings.Count - 1 do
      Result[I] := CXStringToString(Strings.Strings[I], False);
  finally
    clang_disposeStringSet(Strings);
  end;
end;

function TCursor.GetCxxMethodIsConst: Boolean;
begin
  Result := (clang_CXXMethod_isConst(FHandle) <> 0);
end;

function TCursor.GetCxxMethodIsDefaulted: Boolean;
begin
  Result := (clang_CXXMethod_isDefaulted(FHandle) <> 0);
end;

function TCursor.GetCxxMethodIsPureVirtual: Boolean;
begin
  Result := (clang_CXXMethod_isPureVirtual(FHandle) <> 0);
end;

function TCursor.GetCxxMethodIsStatic: Boolean;
begin
  Result := (clang_CXXMethod_isStatic(FHandle) <> 0);
end;

function TCursor.GetCxxMethodIsVirtual: Boolean;
begin
  Result := (clang_CXXMethod_isVirtual(FHandle) <> 0);
end;

function TCursor.GetCxxRecordIsAbstract: Boolean;
begin
  Result := (clang_CXXRecord_isAbstract(FHandle) <> 0);
end;

function TCursor.GetDeclObjCTypeEncoding: String;
begin
  Result := CXStringToString(clang_getDeclObjCTypeEncoding(FHandle));
end;

function TCursor.GetDefinition: TCursor;
begin
  Result.FHandle := clang_getCursorDefinition(FHandle);
end;

function TCursor.GetDisplayName: String;
begin
  Result := CXStringToString(clang_getCursorDisplayName(FHandle));
end;

function TCursor.GetEnumConstantDeclUnsignedValue: UInt64;
begin
  Result := clang_getEnumConstantDeclUnsignedValue(FHandle);
end;

function TCursor.GetEnumConstantDeclValue: Int64;
begin
  Result := clang_getEnumConstantDeclValue(FHandle);
end;

function TCursor.GetEnumDeclIntegerType: TType;
begin
  Result.FHandle := clang_getEnumDeclIntegerType(FHandle);
end;

function TCursor.GetEnumDeclIsScoped: Boolean;
begin
  Result := (clang_EnumDecl_isScoped(FHandle) <> 0);
end;

function TCursor.GetExceptionSpecificationType: TExceptionSpecificationKind;
begin
  Result := TExceptionSpecificationKind(clang_getCursorExceptionSpecificationType(FHandle));
end;

function TCursor.GetExtent: TSourceRange;
begin
  Result.FHandle := clang_getCursorExtent(FHandle);
end;

function TCursor.GetFieldDeclBitWidth: Integer;
begin
  Result := clang_getFieldDeclBitWidth(FHandle);
end;

function TCursor.GetHasAttributes: Boolean;
begin
  Result := (clang_Cursor_hasAttrs(FHandle) <> 0);
end;

function TCursor.GetIBOutletCollectionType: TType;
begin
  Result.FHandle := clang_getIBOutletCollectionType(FHandle);
end;

function TCursor.GetIncludedFile: TFile;
begin
  Result.FHandle := clang_getIncludedFile(FHandle);
end;

function TCursor.GetIsAnonymous: Boolean;
begin
  Result := (clang_Cursor_isAnonymous(FHandle) <> 0);
end;

function TCursor.GetIsAnonymousRecordDecl: Boolean;
begin
  Result := (clang_Cursor_isAnonymousRecordDecl(FHandle) <> 0);
end;

function TCursor.GetIsAttribute: Boolean;
begin
  Result := (clang_isAttribute(clang_getCursorKind(FHandle)) <> 0);
end;

function TCursor.GetIsBitField: Boolean;
begin
  Result := (clang_Cursor_isBitField(FHandle) <> 0);
end;

function TCursor.GetIsDeclaration: Boolean;
begin
  Result := (clang_isDeclaration(clang_getCursorKind(FHandle)) <> 0);
end;

function TCursor.GetIsDefinition: Boolean;
begin
  Result := (clang_isCursorDefinition(FHandle) <> 0);
end;

function TCursor.GetIsDynamicCall: Boolean;
begin
  Result := (clang_Cursor_isDynamicCall(FHandle) <> 0);
end;

function TCursor.GetIsExpression: Boolean;
begin
  Result := (clang_isExpression(clang_getCursorKind(FHandle)) <> 0);
end;

function TCursor.GetIsFunctionInlined: Boolean;
begin
  Result := (clang_Cursor_isFunctionInlined(FHandle) <> 0);
end;

function TCursor.GetIsInlineNamespace: Boolean;
begin
  Result := (clang_Cursor_isInlineNamespace(FHandle) <> 0);
end;

function TCursor.GetIsInvalid: Boolean;
begin
  Result := (clang_isInvalid(clang_getCursorKind(FHandle)) <> 0);
end;

function TCursor.GetIsInvalidDeclaration: Boolean;
begin
  Result := (clang_isInvalidDeclaration(FHandle) <> 0);
end;

function TCursor.GetIsMacroBuiltin: Boolean;
begin
  Result := (clang_Cursor_isMacroBuiltin(FHandle) <> 0);
end;

function TCursor.GetIsMacroFunctionLike: Boolean;
begin
  Result := (clang_Cursor_isMacroFunctionLike(FHandle) <> 0);
end;

function TCursor.GetIsNull: Boolean;
begin
  Result := (clang_Cursor_isNull(FHandle) <> 0);
end;

function TCursor.GetIsObjCOptional: Boolean;
begin
  Result := (clang_Cursor_isObjCOptional(FHandle) <> 0);
end;

function TCursor.GetIsPreprocessing: Boolean;
begin
  Result := (clang_isPreprocessing(clang_getCursorKind(FHandle)) <> 0);
end;

function TCursor.GetIsReference: Boolean;
begin
  Result := (clang_isReference(clang_getCursorKind(FHandle)) <> 0);
end;

function TCursor.GetIsStatement: Boolean;
begin
  Result := (clang_isStatement(clang_getCursorKind(FHandle)) <> 0);
end;

function TCursor.GetIsTranslationUnit: Boolean;
begin
  Result := (clang_isTranslationUnit(clang_getCursorKind(FHandle)) <> 0);
end;

function TCursor.GetIsUnexposed: Boolean;
begin
  Result := (clang_isUnexposed(clang_getCursorKind(FHandle)) <> 0);
end;

function TCursor.GetIsVariadic: Boolean;
begin
  Result := (clang_Cursor_isVariadic(FHandle) <> 0);
end;

function TCursor.GetIsVirtualBase: Boolean;
begin
  Result := (clang_isVirtualBase(FHandle) <> 0);
end;

function TCursor.GetKind: TCursorKind;
begin
  Result := TCursorKind(clang_getCursorKind(FHandle));
end;

function TCursor.GetLanguage: TLanguageKind;
begin
  Result := TLanguageKind(clang_getCursorLanguage(FHandle));
end;

function TCursor.GetLexicalParent: TCursor;
begin
  Result.FHandle := clang_getCursorLexicalParent(FHandle);
end;

function TCursor.GetLinkage: TLinkageKind;
begin
  Result := TLinkageKind(clang_getCursorLinkage(FHandle));
end;

function TCursor.GetLocation: TSourceLocation;
begin
  Result.FHandle := clang_getCursorLocation(FHandle);
end;

function TCursor.GetMangling: String;
begin
  Result := CXStringToString(clang_Cursor_getMangling(FHandle));
end;

function TCursor.GetModule: TModule;
begin
  Result.FHandle := clang_Cursor_getModule(FHandle);
end;

class function TCursor.GetNull: TCursor;
begin
  Result.FHandle := clang_getNullCursor;
end;

function TCursor.GetObjCDeclQualifiers: TObjCDeclQualifierKinds;
begin
  Byte(Result) := clang_Cursor_getObjCDeclQualifiers(FHandle);
end;

function TCursor.GetObjCManglings: TArray<String>;
var
  Strings: PCXStringSet;
  I: Integer;
begin
  Strings := clang_Cursor_getObjCManglings(FHandle);
  try
    SetLength(Result, Strings.Count);
    for I := 0 to Strings.Count - 1 do
      Result[I] := CXStringToString(Strings.Strings[I], False);
  finally
    clang_disposeStringSet(Strings);
  end;
end;

function TCursor.GetObjCPropertyAttributes: TObjCPropertyAttrKinds;
begin
  Word(Result) := clang_Cursor_getObjCPropertyAttributes(FHandle, 0);
end;

function TCursor.GetObjCPropertyGetterName: String;
begin
  Result := CXStringToString(clang_Cursor_getObjCPropertyGetterName(FHandle));
end;

function TCursor.GetObjCPropertySetterName: String;
begin
  Result := CXStringToString(clang_Cursor_getObjCPropertySetterName(FHandle));
end;

function TCursor.GetObjCSelectorIndex: Integer;
begin
  Result := clang_Cursor_getObjCSelectorIndex(FHandle);
end;

function TCursor.GetOffsetOfField: Int64;
begin
  Result := clang_Cursor_getOffsetOfField(FHandle);
end;

function TCursor.GetOverloadedDecl(const AIndex: Integer): TCursor;
begin
  Result.FHandle := clang_getOverloadedDecl(FHandle, AIndex);
end;

function TCursor.GetOverloadedDeclCount: Integer;
begin
  Result := clang_getNumOverloadedDecls(FHandle);
end;

function TCursor.GetOverriddenCursors: TArray<TCursor>;
var
  C: PCXCursor;
  I, Count: Integer;
begin
  clang_getOverriddenCursors(FHandle, @C, @Count);
  SetLength(Result, Count);
  for I := 0 to Count - 1 do
    Result[I].FHandle := C[I];
  clang_disposeOverriddenCursors(C);
end;

function TCursor.GetParsedComment: TComment;
begin
  Result.FHandle := clang_Cursor_getParsedComment(FHandle);
end;

function TCursor.GetPlatformAvailability: TPlatformAvailability;
var
  I, Count: Integer;
  AD, AU: Integer;
  DM, UM: TCXString;
  A: array [0..19] of TCXPlatformAvailability;
  P: PCXPlatformAvailability;
begin
  Count := clang_getCursorPlatformAvailability(FHandle, @AD, @DM, @AU, @UM, @A[0], Length(A));

  Result.AlwaysDeprecated := (AD <> 0);
  Result.DeprecatedMessage := CXStringToString(DM);
  Result.AlwaysUnavailable := (AU <> 0);
  Result.UnavailableMessage := CXStringToString(UM);

  SetLength(Result.Platforms, Count);
  P := @A[0];
  for I := 0 to Count - 1 do
  begin
    with Result.Platforms[I] do
    begin
      Name := CXStringToString(P.Platform, False);
      Introduced := TVersion(P.Introduced);
      DeprecatedVersion := TVersion(P.Deprecated);
      ObsoletedVersion := TVersion(P.Obsoleted);
      Unavailable := (P.Unavailable <> 0);
      Message := CXStringToString(P.Message, False);
    end;
    clang_disposeCXPlatformAvailability(P);
    Inc(P);
  end;
end;

function TCursor.GetPrintingPolicy: IPrintingPolicy;
begin
  Result := TPrintingPolicy.Create(clang_getCursorPrintingPolicy(FHandle));
end;

function TCursor.GetRawComment: String;
begin
  Result := CXStringToString(clang_Cursor_getRawCommentText(FHandle));
end;

function TCursor.GetReceiverType: TType;
begin
  Result.FHandle := clang_Cursor_getReceiverType(FHandle);
end;

function TCursor.GetReferenced: TCursor;
begin
  Result.FHandle := clang_getCursorReferenced(FHandle);
end;

function TCursor.GetReferenceNameRange(const AFlags: TNameRefFlags;
  const APieceIndex: Integer): TSourceRange;
begin
  Result.FHandle := clang_getCursorReferenceNameRange(FHandle, Byte(AFlags), APieceIndex);
end;

function TCursor.GetResultType: TType;
begin
  Result.FHandle := clang_getCursorResultType(FHandle);
end;

function TCursor.GetSemanticParent: TCursor;
begin
  Result.FHandle := clang_getCursorSemanticParent(FHandle);
end;

function TCursor.GetSpecializedCursorTemplate: TCursor;
begin
  Result.FHandle := clang_getSpecializedCursorTemplate(FHandle);
end;

function TCursor.GetSpelling: String;
begin
  Result := CXStringToString(clang_getCursorSpelling(FHandle));
end;

function TCursor.GetSpellingNameRange(const APieceIndex: Integer): TSourceRange;
begin
  Result.FHandle := clang_Cursor_getSpellingNameRange(FHandle, APieceIndex, 0);
end;

function TCursor.GetStorageClass: TStorageClass;
begin
  Result := TStorageClass(clang_Cursor_getStorageClass(FHandle));
end;

function TCursor.GetTemplateArgumentCount: Integer;
begin
  Result := clang_Cursor_getNumTemplateArguments(FHandle);
end;

function TCursor.GetTemplateArgumentKind(
  const AIndex: Integer): TTemplateArgumentKind;
begin
  Result := TTemplateArgumentKind(clang_Cursor_getTemplateArgumentKind(FHandle, AIndex));
end;

function TCursor.GetTemplateArgumentType(const AIndex: Integer): TType;
begin
  Result.FHandle := clang_Cursor_getTemplateArgumentType(FHandle, AIndex);
end;

function TCursor.GetTemplateArgumentUnsignedValue(
  const AIndex: Integer): UInt64;
begin
  Result := clang_Cursor_getTemplateArgumentUnsignedValue(FHandle, AIndex);
end;

function TCursor.GetTemplateArgumentValue(const AIndex: Integer): Int64;
begin
  Result := clang_Cursor_getTemplateArgumentValue(FHandle, AIndex);
end;

function TCursor.GetTemplateCursorKind: TCursorKind;
begin
  Result := TCursorKind(clang_getTemplateCursorKind(FHandle));
end;

function TCursor.GetTlsKind: TTlsKind;
begin
  Result := TTlsKind(clang_getCursorTLSKind(FHandle));
end;

function TCursor.GetTypedefDeclUnderlyingType: TType;
begin
  Result.FHandle := clang_getTypedefDeclUnderlyingType(FHandle);
end;

function TCursor.GetUsr: TUnifiedSymbolResolution;
begin
  Result.FHandle := clang_getCursorUSR(FHandle);
end;

function TCursor.GetVisibility: TVisibility;
begin
  Result := TVisibility(clang_getCursorVisibility(FHandle));
end;

function TCursor.Hash: Cardinal;
begin
  Result := clang_hashCursor(FHandle);
end;

function TCursor.IsExternalSymbol(out ALanguage, ADefinedIn: String;
  out AIsGenerated: Boolean): Boolean;
var
  L, D: TCXString;
  G: Cardinal;
begin
  ALanguage := '';
  ADefinedIn := '';
  AIsGenerated := False;

  Result := (clang_Cursor_isExternalSymbol(FHandle, @L, @D, @G) <> 0);

  if (Result) then
  begin
    ALanguage := CXStringToString(L);
    ADefinedIn := CXStringToString(D);
    AIsGenerated := (G <> 0);
  end;
end;

class operator TCursor.NotEqual(const ALeft, ARight: TCursor): Boolean;
begin
  Result := (clang_equalCursors(ALeft.FHandle, ARight.FHandle) = 0);
end;

function TCursor.PrettyPrinted(const APolicy: IPrintingPolicy): String;
var
  Policy: TCXPrintingPolicy;
begin
  if (APolicy = nil) then
    Policy := nil
  else
    Policy := APolicy.Handle;

  Result := CXStringToString(clang_getCursorPrettyPrinted(FHandle, Policy));
end;

{ TCursorHelper }

class function TCursorHelper.CursorVisitor(ACursor, AParent: TCXCursor;
  AClientData: TCXClientData): TCXChildVisitResult;
var
  Proc: PCursorVisitorProc absolute AClientData;
begin
  if Assigned(Proc.Anon) then
    Result := Ord(Proc.Anon(TCursor(ACursor), TCursor(AParent)))
  else
  begin
    Assert(Assigned(Proc.Meth));
    Result := Ord(Proc.Meth(TCursor(ACursor), TCursor(AParent)))
  end;
end;

function TCursorHelper.FindReferencesInFile(const AFile: TFile;
  const AVisitor: TCursorAndRangeVisitorMethod): TVisitorResult;
var
  Proc: TCursorAndRangeVisitorProc;
  Visitor: TCXCursorAndRangeVisitor;
begin
  Proc.Anon := nil;
  Proc.Meth := AVisitor;

  Visitor.context := @Proc;
  Visitor.visit := CursorAndRangeVisitor;

  Result := TVisitorResult(clang_findReferencesInFile(FHandle, AFile.FHandle, Visitor));
end;

function TCursorHelper.FindReferencesInFile(const AFile: TFile;
  const AVisitor: TCursorAndRangeVisitor): TVisitorResult;
var
  Proc: TCursorAndRangeVisitorProc;
  Visitor: TCXCursorAndRangeVisitor;
begin
  Proc.Anon := AVisitor;
  Proc.Meth := nil;

  Visitor.context := @Proc;
  Visitor.visit := CursorAndRangeVisitor;

  Result := TVisitorResult(clang_findReferencesInFile(FHandle, AFile.FHandle, Visitor));
end;

function TCursorHelper.VisitChildren(const AVisitor: TCursorVisitorMethod): Boolean;
var
  Proc: TCursorVisitorProc;
begin
  Proc.Anon := nil;
  Proc.Meth := AVisitor;

  Result := (clang_visitChildren(FHandle, CursorVisitor, @Proc) <> 0);
end;

function TCursorHelper.VisitChildren(const AVisitor: TCursorVisitor): Boolean;
var
  Proc: TCursorVisitorProc;
begin
  Proc.Anon := AVisitor;
  Proc.Meth := nil;

  Result := (clang_visitChildren(FHandle, CursorVisitor, @Proc) <> 0);
end;

{ TCompletionString }

function TCompletionString.GetAnnotation(const AIndex: Integer): String;
begin
  Result := CXStringToString(clang_getCompletionAnnotation(FHandle, AIndex));
end;

function TCompletionString.GetAnnotationCount: Integer;
begin
  Result := clang_getCompletionNumAnnotations(FHandle);
end;

function TCompletionString.GetAvailability: TAvailabilityKind;
begin
  Result := TAvailabilityKind(clang_getCompletionAvailability(FHandle));
end;

function TCompletionString.GetBriefComment: String;
begin
  Result := CXStringToString(clang_getCompletionBriefComment(FHandle));
end;

function TCompletionString.GetChunkCompletionString(
  const AIndex: Integer): TCompletionString;
begin
  Result.FHandle := clang_getCompletionChunkCompletionString(FHandle, AIndex);
end;

function TCompletionString.GetChunkCount: Integer;
begin
  Result := clang_getNumCompletionChunks(FHandle);
end;

function TCompletionString.GetChunkKind(
  const AIndex: Integer): TCompletionChunkKind;
begin
  Result := TCompletionChunkKind(clang_getCompletionChunkKind(FHandle, AIndex));
end;

function TCompletionString.GetChunkText(const AIndex: Integer): String;
begin
  Result := CXStringToString(clang_getCompletionChunkText(FHandle, AIndex));
end;

function TCompletionString.GetParent: String;
begin
  Result := CXStringToString(clang_getCompletionParent(FHandle, nil));
end;

function TCompletionString.GetPriority: Integer;
begin
  Result := clang_getCompletionPriority(FHandle);
end;

function TCompletionString.IsNull: Boolean;
begin
  Result := (FHandle = nil);
end;

{ TCompilationDatabase }

constructor TCompilationDatabase.Create(const AHandle: TCXCompilationDatabase);
begin
  inherited Create;
  FHandle := AHandle;
end;

destructor TCompilationDatabase.Destroy;
begin
  clang_CompilationDatabase_dispose(FHandle);
  inherited;
end;

class function TCompilationDatabase.FromDirectory(
  const ABuildDir: String): ICompilationDatabase;
var
  CDB: TCXCompilationDatabase;
  Error: TCXCompilationDatabase_Error;
begin
  CDB := clang_CompilationDatabase_fromDirectory(PAnsiChar(AnsiString(ABuildDir)), @Error);
  if (CDB = nil) then
    Result := nil
  else
    Result := TCompilationDatabase.Create(CDB);
end;

function TCompilationDatabase.GetAllCompileCommands: ICompileCommands;
var
  CC: TCXCompileCommands;
begin
  CC := clang_CompilationDatabase_getAllCompileCommands(FHandle);
  if (CC = nil) then
    Result := nil
  else
    Result := TCompileCommands.Create(CC);
end;

function TCompilationDatabase.GetCompileCommands(
  const ACompleteFilename: String): ICompileCommands;
var
  CC: TCXCompileCommands;
begin
  CC := clang_CompilationDatabase_getCompileCommands(FHandle, PAnsiChar(AnsiString(ACompleteFilename)));
  if (CC = nil) then
    Result := nil
  else
    Result := TCompileCommands.Create(CC);
end;

function TCompilationDatabase.GetHandle: TCXCompilationDatabase;
begin
  Result := FHandle;
end;

{ TCompileCommands }

constructor TCompileCommands.Create(const AHandle: TCXCompileCommands);
begin
  inherited Create;
  FHandle := AHandle;
end;

destructor TCompileCommands.Destroy;
begin
  clang_CompileCommands_dispose(FHandle);
  inherited;
end;

function TCompileCommands.GetCommand(const AIndex: Integer): TCompileCommand;
begin
  Result.FHandle := clang_CompileCommands_getCommand(FHandle, AIndex);
end;

function TCompileCommands.GetCount: Integer;
begin
  Result := clang_CompileCommands_getSize(FHandle);
end;

function TCompileCommands.GetHandle: TCXCompileCommands;
begin
  Result := FHandle;
end;

{ TCompileCommand }

function TCompileCommand.GetArg(const AIndex: Integer): String;
begin
  Result := CXStringToString(clang_CompileCommand_getArg(FHandle, AIndex));
end;

function TCompileCommand.GetArgCount: Integer;
begin
  Result := clang_CompileCommand_getNumArgs(FHandle);
end;

function TCompileCommand.GetDirectory: String;
begin
  Result := CXStringToString(clang_CompileCommand_getDirectory(FHandle));
end;

function TCompileCommand.GetFilename: String;
begin
  Result := CXStringToString(clang_CompileCommand_getFilename(FHandle));
end;

function TCompileCommand.GetMappedSourceContent(const AIndex: Integer): String;
begin
  Result := CXStringToString(clang_CompileCommand_getMappedSourceContent(FHandle, AIndex));
end;

function TCompileCommand.GetMappedSourceCount: Integer;
begin
  Result := clang_CompileCommand_getNumMappedSources(FHandle);
end;

function TCompileCommand.GetMappedSourcePath(const AIndex: Integer): String;
begin
  Result := CXStringToString(clang_CompileCommand_getMappedSourcePath(FHandle, AIndex));
end;

{ TVirtualFileOverlay }

function TVirtualFileOverlay.AddFileMapping(const AVirtualPath,
  ARealPath: String): TError;
begin
  Result := TError(clang_VirtualFileOverlay_addFileMapping(FHandle,
    PAnsiChar(AnsiString(AVirtualPath)), PAnsiChar(AnsiString(ARealPath))));
end;

constructor TVirtualFileOverlay.Create;
begin
  inherited Create;
  FHandle := clang_VirtualFileOverlay_create(0);
end;

destructor TVirtualFileOverlay.Destroy;
begin
  clang_VirtualFileOverlay_dispose(FHandle);
  inherited;
end;

function TVirtualFileOverlay.GetHandle: TCXVirtualFileOverlay;
begin
  Result := FHandle;
end;

function TVirtualFileOverlay.SetCaseSensitivity(
  const ACaseSensitive: Boolean): TError;
begin
  Result := TError(clang_VirtualFileOverlay_setCaseSensitivity(FHandle, Ord(ACaseSensitive)));
end;

function TVirtualFileOverlay.WriteToBuffer(out ABuffer: TBytes): TError;
var
  Buffer: PAnsiChar;
  Size: Cardinal;
begin
  Buffer := nil;
  Result := TError(clang_VirtualFileOverlay_writeToBuffer(FHandle, 0, @Buffer, @Size));
  if (Buffer <> nil) then
  try
    SetLength(ABuffer, Size);
    Move(Buffer^, ABuffer[0], Size);
  finally
    clang_free(Buffer);
  end;
end;

{ TModuleMapDescriptor }

constructor TModuleMapDescriptor.Create;
begin
  inherited Create;
  FHandle := clang_ModuleMapDescriptor_create(0);
end;

destructor TModuleMapDescriptor.Destroy;
begin
  clang_ModuleMapDescriptor_dispose(FHandle);
  inherited;
end;

function TModuleMapDescriptor.GetHandle: TCXModuleMapDescriptor;
begin
  Result := FHandle;
end;

function TModuleMapDescriptor.SetFrameworkModuleName(
  const AName: String): TError;
begin
  Result := TError(clang_ModuleMapDescriptor_setFrameworkModuleName(FHandle,
    PAnsiChar(AnsiString(AName))));
end;

function TModuleMapDescriptor.SetUmbrellaHeader(const AName: String): TError;
begin
  Result := TError(clang_ModuleMapDescriptor_setUmbrellaHeader(FHandle,
    PAnsiChar(AnsiString(AName))));
end;

function TModuleMapDescriptor.WriteToBuffer(out ABuffer: TBytes): TError;
var
  Buffer: PAnsiChar;
  Size: Cardinal;
begin
  Buffer := nil;
  Result := TError(clang_ModuleMapDescriptor_writeToBuffer(FHandle, 0, @Buffer, @Size));
  if (Buffer <> nil) then
  try
    SetLength(ABuffer, Size);
    Move(Buffer^, ABuffer[0], Size);
  finally
    clang_free(Buffer);
  end;
end;

{ TSourceRangeList }

constructor TSourceRangeList.Create(const AHandle: PCXSourceRangeList);
begin
  inherited Create;
  FHandle := AHandle;
end;

destructor TSourceRangeList.Destroy;
begin
  clang_disposeSourceRangeList(FHandle);
  inherited;
end;

function TSourceRangeList.GetCount: Integer;
begin
  Result := FHandle.count;
end;

function TSourceRangeList.GetHandle: PCXSourceRangeList;
begin
  Result := FHandle;
end;

function TSourceRangeList.GetRange(const AIndex: Integer): TSourceRange;
begin
  Result.FHandle := FHandle.ranges[AIndex];
end;

{ TDiagnosticSet }

constructor TDiagnosticSet.Create(const AHandle: TCXDiagnosticSet;
  const AOwnsHandle: Boolean);
begin
  inherited Create;
  FHandle := AHandle;
  FOwnsHandle := AOwnsHandle;
end;

destructor TDiagnosticSet.Destroy;
begin
  if (FOwnsHandle) then
    clang_disposeDiagnosticSet(FHandle);
  inherited;
end;

function TDiagnosticSet.GetCount: Integer;
begin
  Result := clang_getNumDiagnosticsInSet(FHandle);
end;

function TDiagnosticSet.GetDiagnostic(const AIndex: Integer): IDiagnostic;
var
  D: TCXDiagnostic;
begin
  D := clang_getDiagnosticInSet(FHandle, AIndex);
  if (D = nil) then
    Result := nil
  else
    Result := TDiagnostic.Create(D);
end;

function TDiagnosticSet.GetHandle: TCXDiagnosticSet;
begin
  Result := FHandle;
end;

function TDiagnosticSet.Load(const AFilename: String;
  out AError: TLoadDiagError; out AErrorString: String): IDiagnosticSet;
var
  DS: TDiagnosticSet;
  Error: TCXLoadDiag_Error;
  ErrorString: TCXString;
begin
  DS := clang_loadDiagnostics(PAnsiChar(AnsiString(AFilename)), @Error, @ErrorString);
  AError := TLoadDiagError(Error);
  AErrorString := CXStringToString(ErrorString);
  if (DS = nil) then
    Result := nil
  else
    Result := TDiagnosticSet.Create(DS);
end;

function TDiagnosticSet.Load(const AFilename: String): IDiagnosticSet;
var
  Error: TLoadDiagError;
  ErrorString: String;
begin
  Result := Load(AFilename, Error, ErrorString);
end;

{ TResourceUsageEntry }

function TResourceUsageEntry.GetAmount: Integer;
begin
  Result := FHandle.amount;
end;

function TResourceUsageEntry.GetKind: TResourceUsageKind;
begin
  Result := TResourceUsageKind(FHandle.kind);
end;

function TResourceUsageEntry.GetName: String;
begin
  Result := String(AnsiString(clang_getTUResourceUsageName(FHandle.kind)));
end;

{ TCursorSet }

function TCursorSet.Contains(const ACursor: TCursor): Boolean;
begin
  Result := (clang_CXCursorSet_contains(FHandle, ACursor.FHandle) <> 0);
end;

constructor TCursorSet.Create;
begin
  FHandle := clang_createCXCursorSet;
end;

destructor TCursorSet.Destroy;
begin
  clang_disposeCXCursorSet(FHandle);
  inherited;
end;

function TCursorSet.GetHandle: TCXCursorSet;
begin
  Result := FHandle;
end;

function TCursorSet.Insert(const ACursor: TCursor): Boolean;
begin
  Result := (clang_CXCursorSet_insert(FHandle, ACursor.FHandle) <> 0);
end;

{ TType }

class operator TType.Equal(const ALeft, ARight: TType): Boolean;
begin
  Result := (clang_equalTypes(ALeft.FHandle, ARight.FHandle) <> 0);
end;

function TType.GetAddressSpace: Cardinal;
begin
  Result := clang_getAddressSpace(FHandle);
end;

function TType.GetAlignOf: Int64;
begin
  Result := clang_Type_getAlignOf(FHandle);
end;

function TType.GetArgType(const AIndex: Integer): TType;
begin
  Result.FHandle := clang_getArgType(FHandle, AIndex);
end;

function TType.GetArgTypeCount: Integer;
begin
  Result := clang_getNumArgTypes(FHandle);
end;

function TType.GetArrayElementType: TType;
begin
  Result.FHandle := clang_getArrayElementType(FHandle);
end;

function TType.GetArraySize: Int64;
begin
  Result := clang_getArraySize(FHandle);
end;

function TType.GetCanonicalType: TType;
begin
  Result.FHandle := clang_getCanonicalType(FHandle);
end;

function TType.GetClassType: TType;
begin
  Result.FHandle := clang_Type_getClassType(FHandle);
end;

function TType.GetCxxRefQualifier: TRefQualifierKind;
begin
  Result := TRefQualifierKind(clang_Type_getCXXRefQualifier(FHandle));
end;

function TType.GetElementCount: Integer;
begin
  Result := clang_getNumElements(FHandle);
end;

function TType.GetElementType: TType;
begin
  Result.FHandle := clang_getElementType(FHandle);
end;

function TType.GetExceptionSpecificationType: TExceptionSpecificationKind;
begin
  Result := TExceptionSpecificationKind(clang_getExceptionSpecificationType(FHandle));
end;

function TType.GetFunctionCallingConv: TCallingConv;
begin
  Result := TCallingConv(clang_getFunctionTypeCallingConv(FHandle));
end;

function TType.GetIsConstQualified: Boolean;
begin
  Result := (clang_isConstQualifiedType(FHandle) <> 0);
end;

function TType.GetIsFunctionVariadic: Boolean;
begin
  Result := (clang_isFunctionTypeVariadic(FHandle) <> 0);
end;

function TType.GetIsPodType: Boolean;
begin
  Result := (clang_isPODType(FHandle) <> 0);
end;

function TType.GetIsRestrictQualified: Boolean;
begin
  Result := (clang_isRestrictQualifiedType(FHandle) <> 0);
end;

function TType.GetIsTransparentTagTypedef: Boolean;
begin
  Result := (clang_Type_isTransparentTagTypedef(FHandle) <> 0);
end;

function TType.GetIsVolatileQualified: Boolean;
begin
  Result := (clang_isVolatileQualifiedType(FHandle) <> 0);
end;

function TType.GetKind: TTypeKind;
begin
  Result := TTypeKind(FHandle.kind);
end;

function TType.GetKindSpelling: String;
begin
  Result := CXStringToString(clang_getTypeKindSpelling(FHandle.kind));
end;

function TType.GetModifiedType: TType;
begin
  Result.FHandle := clang_Type_getModifiedType(FHandle);
end;

function TType.GetNamedType: TType;
begin
  Result.FHandle := clang_Type_getNamedType(FHandle);
end;

function TType.GetNullability: TTypeNullabilityKind;
begin
  Result := TTypeNullabilityKind(clang_Type_getNullability(FHandle));
end;

function TType.GetObjCEncoding: String;
begin
  Result := CXStringToString(clang_Type_getObjCEncoding(FHandle));
end;

function TType.GetObjCObjectBaseType: TType;
begin
  Result.FHandle := clang_Type_getObjCObjectBaseType(FHandle);
end;

function TType.GetObjCProtocolDecl(const AIndex: Integer): TCXCursor;
begin
  Result := clang_Type_getObjCProtocolDecl(FHandle, AIndex);
end;

function TType.GetObjCProtocolRefCount: Integer;
begin
  Result := clang_Type_getNumObjCProtocolRefs(FHandle);
end;

function TType.GetObjCTypeArg(const AIndex: Integer): TType;
begin
  Result.FHandle := clang_Type_getObjCTypeArg(FHandle, AIndex);
end;

function TType.GetObjCTypeArgCount: Integer;
begin
  Result := clang_Type_getNumObjCTypeArgs(FHandle);
end;

function TType.GetOffsetOf(const AFieldName: String): Int64;
begin
  Result := clang_Type_getOffsetOf(FHandle, PAnsiChar(AnsiString(AFieldName)));
end;

function TType.GetPointeeType: TType;
begin
  Result.FHandle := clang_getPointeeType(FHandle);
end;

function TType.GetResultType: TType;
begin
  Result.FHandle := clang_getResultType(FHandle);
end;

function TType.GetSizeOf: Int64;
begin
  Result := clang_Type_getSizeOf(FHandle);
end;

function TType.GetSpelling: String;
begin
  Result := CXStringToString(clang_getTypeSpelling(FHandle));
end;

function TType.GetTemplateArgumentCount: Integer;
begin
  Result := clang_Type_getNumTemplateArguments(FHandle);
end;

function TType.GetTemplateArgumentType(const AIndex: Integer): TType;
begin
  Result.FHandle := clang_Type_getTemplateArgumentAsType(FHandle, AIndex);
end;

function TType.GetTypedefName: String;
begin
  Result := CXStringToString(clang_getTypedefName(FHandle));
end;

class operator TType.NotEqual(const ALeft, ARight: TType): Boolean;
begin
  Result := (clang_equalTypes(ALeft.FHandle, ARight.FHandle) = 0);
end;

{ TTypeHelper }

class function TTypeHelper.FieldVisitor(ACursor: TCXCursor;
  AClientData: TCXClientData): TCXVisitorResult;
var
  Proc: PFieldVisitorProc absolute AClientData;
begin
  if Assigned(Proc.Anon) then
    Result := Ord(Proc.Anon(TCursor(ACursor)))
  else
  begin
    Assert(Assigned(Proc.Meth));
    Result := Ord(Proc.Meth(TCursor(ACursor)))
  end;
end;

function TTypeHelper.GetDeclaration: TCursor;
begin
  Result.FHandle := clang_getTypeDeclaration(FHandle);
end;

function TTypeHelper.VisitFields(const AVisitor: TFieldVisitorMethod): Boolean;
var
  Proc: TFieldVisitorProc;
begin
  Proc.Anon := nil;
  Proc.Meth := AVisitor;

  Result := (clang_Type_visitFields(FHandle, FieldVisitor, @Proc) <> 0);
end;

function TTypeHelper.VisitFields(const AVisitor: TFieldVisitor): Boolean;
var
  Proc: TFieldVisitorProc;
begin
  Proc.Anon := AVisitor;
  Proc.Meth := nil;

  Result := (clang_Type_visitFields(FHandle, FieldVisitor, @Proc) <> 0);
end;

{ TUnifiedSymbolResolution }

class operator TUnifiedSymbolResolution.Equal(const ALeft,
  ARight: TUnifiedSymbolResolution): Boolean;
begin
  {$IFDEF WIN32}
  Result := (ALeft.FHandle = ARight.FHandle);
  {$ELSE}
  Result := (ALeft.FHandle.data = ARight.FHandle.data)
        and (ALeft.FHandle.private_flags = ARight.FHandle.private_flags);
  {$ENDIF}
end;

class operator TUnifiedSymbolResolution.NotEqual(const ALeft,
  ARight: TUnifiedSymbolResolution): Boolean;
begin
  Result := not (ALeft = ARight);
end;

function TUnifiedSymbolResolution.ToString: String;
begin
  Result := CXStringToString(FHandle, False);
end;

{ TToken }

function TToken.GetKind: TTokenKind;
begin
  Result := TTokenKind(clang_getTokenKind(FHandle));
end;

{ TCursorKindHelper }

function TCursorKindHelper.GetSpelling: String;
begin
  Result := CXStringToString(clang_getCursorKindSpelling(Ord(Self)));
end;

{ TCompletionResult }

function TCompletionResult.GetCompletionString: TCompletionString;
begin
  Result.FHandle := FHandle.CompletionString;
end;

function TCompletionResult.GetKind: TCursorKind;
begin
  Result := TCursorKind(FHandle.CursorKind);
end;

class procedure TCompletionResult.Sort(var AResults: TArray<TCompletionResult>);
begin
  clang_sortCodeCompletionResults(@AResults[0], Length(AResults));
end;

{ TEvalResult }

constructor TEvalResult.Create(const AHandle: TCXEvalResult);
begin
  inherited Create;
  FHandle := AHandle;
end;

destructor TEvalResult.Destroy;
begin
  clang_EvalResult_dispose(FHandle);
  inherited;
end;

function TEvalResult.GetAsDouble: Double;
begin
  Result := clang_EvalResult_getAsDouble(FHandle);
end;

function TEvalResult.GetAsInt: Integer;
begin
  Result := clang_EvalResult_getAsInt(FHandle);
end;

function TEvalResult.GetAsInt64: Int64;
begin
  Result := clang_EvalResult_getAsLongLong(FHandle);
end;

function TEvalResult.GetAsString: String;
begin
  Result := String(AnsiString(clang_EvalResult_getAsStr(FHandle)));
end;

function TEvalResult.GetAsUnsigned: UInt64;
begin
  Result := clang_EvalResult_getAsUnsigned(FHandle);
end;

function TEvalResult.GetHandle: TCXEvalResult;
begin
  Result := FHandle;
end;

function TEvalResult.GetIsUnsignedInt: Boolean;
begin
  Result := (clang_EvalResult_isUnsignedInt(FHandle) <> 0);
end;

function TEvalResult.GetKind: TEvalResultKind;
begin
  Result := TEvalResultKind(clang_EvalResult_getKind(FHandle));
end;

{ TPrintingPolicy }

constructor TPrintingPolicy.Create(const AHandle: TCXPrintingPolicy);
begin
  inherited Create;
  FHandle := AHandle;
end;

destructor TPrintingPolicy.Destroy;
begin
  clang_PrintingPolicy_dispose(FHandle);
  inherited;
end;

function TPrintingPolicy.GetHandle: TCXPrintingPolicy;
begin
  Result := FHandle;
end;

function TPrintingPolicy.GetProperty(
  const AProp: TPrintingPolicyProperty): Integer;
begin
  Result := clang_PrintingPolicy_getProperty(FHandle, Ord(AProp));
end;

procedure TPrintingPolicy.SetProperty(const AProp: TPrintingPolicyProperty;
  const AValue: Integer);
begin
  clang_PrintingPolicy_setProperty(FHandle, Ord(AProp), AValue);
end;

{ TRemapping }

class function TRemapping.Create(const AFilePaths: array of String): IRemapping;
var
  Handle: TCXRemapping;
  PathStrings: TArray<AnsiString>;
  PathPtrs: TArray<PAnsiChar>;
  I: Integer;
begin
  SetLength(PathStrings, Length(AFilePaths));
  SetLength(PathPtrs, Length(AFilePaths));
  for I := 0 to Length(AFilePaths) - 1 do
  begin
    PathStrings[I] := AnsiString(AFilePaths[I]);
    PathPtrs[I] := PAnsiChar(PathStrings[I]);
  end;

  Handle := clang_getRemappingsFromFileList(@PathPtrs[0], Length(PathPtrs));
  if (Handle = nil) then
    Result := nil
  else
    Result := TRemapping.Create(Handle);
end;

class function TRemapping.Create(const APath: String): IRemapping;
var
  Handle: TCXRemapping;
begin
  Handle := clang_getRemappings(PAnsiChar(AnsiString(APath)));
  if (Handle = nil) then
    Result := nil
  else
    Result := TRemapping.Create(Handle);
end;

constructor TRemapping.Create(const AHandle: TCXRemapping);
begin
  inherited Create;
  FHandle := AHandle;
end;

destructor TRemapping.Destroy;
begin
  clang_remap_dispose(FHandle);
  inherited;
end;

function TRemapping.GetAssociatedFilename(const AIndex: Integer): String;
var
  S: TCXString;
begin
  clang_remap_getFilenames(FHandle, AIndex, nil, @S);
  Result := CXStringToString(S);
end;

function TRemapping.GetCount: Integer;
begin
  Result := clang_remap_getNumFiles(FHandle);
end;

function TRemapping.GetHandle: TCXRemapping;
begin
  Result := FHandle;
end;

function TRemapping.GetOriginalFilename(const AIndex: Integer): String;
var
  S: TCXString;
begin
  clang_remap_getFilenames(FHandle, AIndex, @S, nil);
  Result := CXStringToString(S);
end;

{ TIdxIncludedFileInfo }

function TIdxIncludedFileInfo.GetFilename: String;
begin
  Result := String(AnsiString(FHandle.filename));
end;

function TIdxIncludedFileInfo.GetHashLoc: TIdxLoc;
begin
  Result.FHandle := FHandle.hashLoc;
end;

function TIdxIncludedFileInfo.GetIncludedFile: TFile;
begin
  Result.FHandle := FHandle._file
end;

function TIdxIncludedFileInfo.GetIsAngled: Boolean;
begin
  Result := (FHandle.isAngled <> 0);
end;

function TIdxIncludedFileInfo.GetIsImport: Boolean;
begin
  Result := (FHandle.isImport <> 0);
end;

function TIdxIncludedFileInfo.GetIsModuleImport: Boolean;
begin
  Result := (FHandle.isModuleImport <> 0);
end;

{ TIdxImportedAstFileInfo }

function TIdxImportedAstFileInfo.GetAstFile: TFile;
begin
  Result.FHandle := FHandle._file;
end;

function TIdxImportedAstFileInfo.GetIsImplicit: Boolean;
begin
  Result := (FHandle.isImplicit <> 0);
end;

function TIdxImportedAstFileInfo.GetLoc: TIdxLoc;
begin
  Result.FHandle := FHandle.loc;
end;

function TIdxImportedAstFileInfo.GetModule: TModule;
begin
  Result.FHandle := FHandle.module;
end;

{ TIdxAttrInfo }

function TIdxAttrInfo.GetCursor: TCursor;
begin
  Result.FHandle := FHandle.cursor;
end;

function TIdxAttrInfo.GetKind: TIdxAttrKind;
begin
  Result := TIdxAttrKind(FHandle.kind);
end;

function TIdxAttrInfo.GetLoc: TIdxLoc;
begin
  Result.FHandle := FHandle.loc;
end;

{ TIdxEntityInfo }

function TIdxEntityInfo.GetAttributes: TArray<TIdxAttrInfo>;
var
  I: Integer;
begin
  SetLength(Result, FHandle.numAttributes);
  for I := 0 to Length(Result) - 1 do
    Result[I].FHandle := FHandle.attributes[I];
end;

function TIdxEntityInfo.GetClientEntity: TIdxClientEntity;
begin
  Result := clang_index_getClientEntity(FHandle);
end;

function TIdxEntityInfo.GetCursor: TCursor;
begin
  Result.FHandle := FHandle.cursor;
end;

function TIdxEntityInfo.GetIsObjCContainer: Boolean;
begin
  Result := (clang_index_isEntityObjCContainerKind(FHandle.kind) <> 0);
end;

function TIdxEntityInfo.GetKind: TIdxEntityKind;
begin
  Result := TIdxEntityKind(FHandle.kind);
end;

function TIdxEntityInfo.GetLang: TIdxEntityLanguage;
begin
  Result := TIdxEntityLanguage(FHandle.lang);
end;

function TIdxEntityInfo.GetName: String;
begin
  Result := String(AnsiString(FHandle.name));
end;

function TIdxEntityInfo.GetTemplateKind: TIdxEntityCxxTemplateKind;
begin
  Result := TIdxEntityCxxTemplateKind(FHandle.templateKind);
end;

function TIdxEntityInfo.GetUsr: String;
begin
  Result := String(AnsiString(FHandle.USR));
end;

procedure TIdxEntityInfo.SetClientEntity(const AValue: TIdxClientEntity);
begin
  clang_index_setClientEntity(FHandle, AValue);
end;

{ TIdxContainerInfo }

function TIdxContainerInfo.GetClientContainer: TIdxClientContainer;
begin
  Result := clang_index_getClientContainer(FHandle);
end;

function TIdxContainerInfo.GetCursor: TCursor;
begin
  Result.FHandle := FHandle.cursor;
end;

procedure TIdxContainerInfo.SetClientContainer(
  const AValue: TIdxClientContainer);
begin
  clang_index_setClientContainer(FHandle, AValue);
end;

{ TIdxDeclInfo }

function TIdxDeclInfo.GetAttributes: TArray<TIdxAttrInfo>;
var
  I: Integer;
begin
  SetLength(Result, FHandle.numAttributes);
  for I := 0 to Length(Result) - 1 do
    Result[I].FHandle := FHandle.attributes[I];
end;

function TIdxDeclInfo.GetCursor: TCursor;
begin
  Result.FHandle := FHandle.cursor;
end;

function TIdxDeclInfo.GetDeclAsContainer: TIdxContainerInfo;
begin
  Result.FHandle := FHandle.declAsContainer;
end;

function TIdxDeclInfo.GetEntityInfo: TIdxEntityInfo;
begin
  Result.FHandle := FHandle.entityInfo;
end;

function TIdxDeclInfo.GetFlags: Cardinal;
begin
  Result := FHandle.flags;
end;

function TIdxDeclInfo.GetIsContainer: Boolean;
begin
  Result := (FHandle.isContainer <> 0);
end;

function TIdxDeclInfo.GetIsDefinition: Boolean;
begin
  Result := (FHandle.isDefinition <> 0);
end;

function TIdxDeclInfo.GetIsImplicit: Boolean;
begin
  Result := (FHandle.isImplicit <> 0);
end;

function TIdxDeclInfo.GetIsRedeclaration: Boolean;
begin
  Result := (FHandle.isRedeclaration <> 0);
end;

function TIdxDeclInfo.GetLexicalContainer: TIdxContainerInfo;
begin
  Result.FHandle := FHandle.lexicalContainer;
end;

function TIdxDeclInfo.GetLoc: TIdxLoc;
begin
  Result.FHandle := FHandle.loc;
end;

function TIdxDeclInfo.GetSemanticContainer: TIdxContainerInfo;
begin
  Result.FHandle := FHandle.semanticContainer;
end;

{ TIdxEntityRefInfo }

function TIdxEntityRefInfo.GetContainer: TIdxContainerInfo;
begin
  Result.FHandle := FHandle.container;
end;

function TIdxEntityRefInfo.GetCursor: TCursor;
begin
  Result.FHandle := FHandle.cursor;
end;

function TIdxEntityRefInfo.GetKind: TIdxEntityRefKind;
begin
  Result := TIdxEntityRefKind(FHandle.kind);
end;

function TIdxEntityRefInfo.GetLoc: TIdxLoc;
begin
  Result.FHandle := FHandle.loc;
end;

function TIdxEntityRefInfo.GetParentEntity: TIdxEntityInfo;
begin
  Result.FHandle := FHandle.parentEntity;
end;

function TIdxEntityRefInfo.GetReferencedEntity: TIdxEntityInfo;
begin
  Result.FHandle := FHandle.referencedEntity;
end;

function TIdxEntityRefInfo.GetRoles: TSymbolRoles;
begin
  Word(Result) := FHandle.role;
end;

{ TIdxObjCContainerDeclInfo }

function TIdxObjCContainerDeclInfo.GetDeclInfo: TIdxDeclInfo;
begin
  Result.FHandle := FHandle.declInfo;
end;

function TIdxObjCContainerDeclInfo.GetKind: TIdxObjCContainerKind;
begin
  Result := TIdxObjCContainerKind(FHandle.kind);
end;

{ TIdxDeclInfoHelper }

function TIdxDeclInfoHelper.GetCxxClassDeclInfo: TIdxCxxClassDeclInfo;
begin
  Result.FHandle := clang_index_getCXXClassDeclInfo(FHandle);
end;

function TIdxDeclInfoHelper.GetObjCCategoryDeclInfo: TIdxObjCCategoryDeclInfo;
begin
  Result.FHandle := clang_index_getObjCCategoryDeclInfo(FHandle);
end;

function TIdxDeclInfoHelper.GetObjCContainerDeclInfo: TIdxObjCContainerDeclInfo;
begin
  Result.FHandle := clang_index_getObjCContainerDeclInfo(FHandle);
end;

function TIdxDeclInfoHelper.GetObjCInterfaceDeclInfo: TIdxObjCInterfaceDeclInfo;
begin
  Result.FHandle := clang_index_getObjCInterfaceDeclInfo(FHandle);
end;

function TIdxDeclInfoHelper.GetObjCPropertyDeclInfo: TIdxObjCPropertyDeclInfo;
begin
  Result.FHandle := clang_index_getObjCPropertyDeclInfo(FHandle);
end;

function TIdxDeclInfoHelper.GetObjCProtocolRefListInfo: TIdxObjCProtocolRefListInfo;
begin
  Result.FHandle := clang_index_getObjCProtocolRefListInfo(FHandle);
end;

{ TIdxBaseClassInfo }

function TIdxBaseClassInfo.GetBase: TIdxEntityInfo;
begin
  Result.FHandle := FHandle.base;
end;

function TIdxBaseClassInfo.GetCursor: TCursor;
begin
  Result.FHandle := FHandle.cursor;
end;

function TIdxBaseClassInfo.GetLoc: TIdxLoc;
begin
  Result.FHandle := FHandle.loc;
end;

{ TIdxObjCProtocolRefInfo }

function TIdxObjCProtocolRefInfo.GetCursor: TCursor;
begin
  Result.FHandle := FHandle.cursor;
end;

function TIdxObjCProtocolRefInfo.GetLoc: TIdxLoc;
begin
  Result.FHandle := FHandle.loc;
end;

function TIdxObjCProtocolRefInfo.GetProtocol: TIdxEntityInfo;
begin
  Result.FHandle := FHandle.protocol;
end;

{ TIdxObjCProtocolRefListInfo }

function TIdxObjCProtocolRefListInfo.GetProtocols: TArray<TIdxObjCProtocolRefInfo>;
var
  I: Integer;
begin
  SetLength(Result, FHandle.numProtocols);
  for I := 0 to Length(Result) - 1 do
    Result[I].FHandle := FHandle.protocols[I];
end;

{ TIdxObjCInterfaceDeclInfo }

function TIdxObjCInterfaceDeclInfo.GetContainerInfo: TIdxObjCContainerDeclInfo;
begin
  Result.FHandle := FHandle.containerInfo;
end;

function TIdxObjCInterfaceDeclInfo.GetProtocols: TIdxObjCProtocolRefListInfo;
begin
  Result.FHandle := FHandle.protocols;
end;

function TIdxObjCInterfaceDeclInfo.GetSuperInfo: TIdxBaseClassInfo;
begin
  Result.FHandle := FHandle.superInfo;
end;

{ TIdxObjCCategoryDeclInfo }

function TIdxObjCCategoryDeclInfo.GetClassCursor: TCursor;
begin
  Result.FHandle := FHandle.classCursor;
end;

function TIdxObjCCategoryDeclInfo.GetClassLoc: TIdxLoc;
begin
  Result.FHandle := FHandle.classLoc;
end;

function TIdxObjCCategoryDeclInfo.GetContainerInfo: TIdxObjCContainerDeclInfo;
begin
  Result.FHandle := FHandle.containerInfo;
end;

function TIdxObjCCategoryDeclInfo.GetObjCClass: TIdxEntityInfo;
begin
  Result.FHandle := FHandle.objcClass;
end;

function TIdxObjCCategoryDeclInfo.GetProtocols: TIdxObjCProtocolRefListInfo;
begin
  Result.FHandle := FHandle.protocols;
end;

{ TIdxObjCPropertyDeclInfo }

function TIdxObjCPropertyDeclInfo.GetDeclInfo: TIdxDeclInfo;
begin
  Result.FHandle := FHandle.declInfo;
end;

function TIdxObjCPropertyDeclInfo.GetGetter: TIdxEntityInfo;
begin
  Result.FHandle := FHandle.getter;
end;

function TIdxObjCPropertyDeclInfo.GetSetter: TIdxEntityInfo;
begin
  Result.FHandle := FHandle.setter;
end;

{ TIdxIBOutletCollectionAttrInfo }

function TIdxIBOutletCollectionAttrInfo.GetAttrInfo: TIdxAttrInfo;
begin
  Result.FHandle := FHandle.attrInfo;
end;

function TIdxIBOutletCollectionAttrInfo.GetClassCursor: TCursor;
begin
  Result.FHandle := FHandle.classCursor;
end;

function TIdxIBOutletCollectionAttrInfo.GetClassLoc: TIdxLoc;
begin
  Result.FHandle := FHandle.classLoc;
end;

function TIdxIBOutletCollectionAttrInfo.GetObjCClass: TIdxEntityInfo;
begin
  Result.FHandle := FHandle.objcClass;
end;

{ TIdxAttrInfoHelper }

function TIdxAttrInfoHelper.GetIBOutletCollectionAttrInfo: TIdxIBOutletCollectionAttrInfo;
begin
  Result.FHandle := clang_index_getIBOutletCollectionAttrInfo(FHandle);
end;

{ TIdxCxxClassDeclInfo }

function TIdxCxxClassDeclInfo.GetBases: TArray<TIdxBaseClassInfo>;
var
  I: Integer;
begin
  SetLength(Result, FHandle.numBases);
  for I := 0 to Length(Result) - 1 do
    Result[I].FHandle := FHandle.bases[I];
end;

function TIdxCxxClassDeclInfo.GetDeclInfo: TIdxDeclInfo;
begin
  Result.FHandle := FHandle.declInfo;
end;

{ TIdxLoc }

procedure TIdxLoc.GetFileLocation(out AIndexFile: TIdxClientFile;
  out AFile: TFile; out ALine, AColumn, AOffset: Integer);
begin
  clang_indexLoc_getFileLocation(FHandle, @AIndexFile, @AFile.FHandle, @ALine,
    @AColumn, @AOffset);
end;

function TIdxLoc.GetSourceLocation: TSourceLocation;
begin
  Result.FHandle := clang_indexLoc_getCXSourceLocation(FHandle);
end;

{ TComment }

function TComment.GetBlockCommandArg(const AIndex: Integer): String;
begin
  Result := CXStringToString(clang_BlockCommandComment_getArgText(FHandle, AIndex));
end;

function TComment.GetBlockCommandArgCount: Integer;
begin
  Result := clang_BlockCommandComment_getNumArgs(FHandle);
end;

function TComment.GetBlockCommandName: String;
begin
  Result := CXStringToString(clang_BlockCommandComment_getCommandName(FHandle));
end;

function TComment.GetBlockCommandParagraph: TComment;
begin
  Result.FHandle := clang_BlockCommandComment_getParagraph(FHandle);
end;

function TComment.GetChild(const AIndex: Integer): TComment;
begin
  Result.FHandle := clang_Comment_getChild(FHandle, AIndex);
end;

function TComment.GetChildCount: Integer;
begin
  Result := clang_Comment_getNumChildren(FHandle);
end;

function TComment.GetFullCommentAsHtml: String;
begin
  Result := CXStringToString(clang_FullComment_getAsHTML(FHandle));
end;

function TComment.GetFullCommentASXml: String;
begin
  Result := CXStringToString(clang_FullComment_getAsXML(FHandle));
end;

function TComment.GetHasTrailingNewline: Boolean;
begin
  Result := (clang_InlineContentComment_hasTrailingNewline(FHandle) <> 0);
end;

function TComment.GetHtmlTagAsString: String;
begin
  Result := CXStringToString(clang_HTMLTagComment_getAsString(FHandle));
end;

function TComment.GetHtmlTagAttrCount: Integer;
begin
  Result := clang_HTMLStartTag_getNumAttrs(FHandle);
end;

function TComment.GetHtmlTagAttrName(const AIndex: Integer): String;
begin
  Result := CXStringToString(clang_HTMLStartTag_getAttrName(FHandle, AIndex));
end;

function TComment.GetHtmlTagAttrValue(const AIndex: Integer): String;
begin
  Result := CXStringToString(clang_HTMLStartTag_getAttrValue(FHandle, AIndex));
end;

function TComment.GetHtmlTagIsSelfClosing: Boolean;
begin
  Result := (clang_HTMLStartTagComment_isSelfClosing(FHandle) <> 0);
end;

function TComment.GetHtmlTagName: String;
begin
  Result := CXStringToString(clang_HTMLTagComment_getTagName(FHandle));
end;

function TComment.GetInlineCommandArg(const AIndex: Integer): String;
begin
  Result := CXStringToString(clang_InlineCommandComment_getArgText(FHandle, AIndex));
end;

function TComment.GetInlineCommandArgCount: Integer;
begin
  Result := clang_InlineCommandComment_getNumArgs(FHandle);
end;

function TComment.GetInlineCommandName: String;
begin
  Result := CXStringToString(clang_InlineCommandComment_getCommandName(FHandle));
end;

function TComment.GetInlineCommandRenderKind: TCommentInlineCommandRenderKind;
begin
  Result := TCommentInlineCommandRenderKind(clang_InlineCommandComment_getRenderKind(FHandle));
end;

function TComment.GetIsWhitespace: Boolean;
begin
  Result := (clang_Comment_isWhitespace(FHandle) <> 0);
end;

function TComment.GetKind: TCommentKind;
begin
  Result := TCommentKind(clang_Comment_getKind(FHandle));
end;

function TComment.GetParamCommandDirection: TCommentParamPassDirection;
begin
  Result := TCommentParamPassDirection(clang_ParamCommandComment_getDirection(FHandle));
end;

function TComment.GetParamCommandIsDirectionExplicit: Boolean;
begin
  Result := (clang_ParamCommandComment_isDirectionExplicit(FHandle) <> 0);
end;

function TComment.GetParamCommandIsParamIndexValid: Boolean;
begin
  Result := (clang_ParamCommandComment_isParamIndexValid(FHandle) <> 0);
end;

function TComment.GetParamCommandParamIndex: Integer;
begin
  Result := clang_ParamCommandComment_getParamIndex(FHandle);
end;

function TComment.GetParamCommandParamName: String;
begin
  Result := CXStringToString(clang_ParamCommandComment_getParamName(FHandle));
end;

function TComment.GetText: String;
begin
  Result := CXStringToString(clang_TextComment_getText(FHandle));
end;

function TComment.GetTParamCommandDepth: Integer;
begin
  Result := clang_TParamCommandComment_getDepth(FHandle);
end;

function TComment.GetTParamCommandIndex(const ADepth: Integer): Integer;
begin
  Result := clang_TParamCommandComment_getIndex(FHandle, ADepth);
end;

function TComment.GetTParamCommandIsParamPositionValied: Boolean;
begin
  Result := (clang_TParamCommandComment_isParamPositionValid(FHandle) <> 0);
end;

function TComment.GetTParamCommandParamName: String;
begin
  Result := CXStringToString(clang_TParamCommandComment_getParamName(FHandle));
end;

function TComment.GetVerbatimBlockLineText: String;
begin
  Result := CXStringToString(clang_VerbatimBlockLineComment_getText(FHandle));
end;

function TComment.GetVerbatimLineText: String;
begin
  Result := CXStringToString(clang_VerbatimLineComment_getText(FHandle));
end;

end.
