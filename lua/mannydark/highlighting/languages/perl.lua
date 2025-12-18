-------------------------------------------------------------------------------
-- Perl Files
-- Highlighting for .pl, .pm, .t, .pod files.
-------------------------------------------------------------------------------

local colors  = require('mannydark.palette')
local highlight = vim.api.nvim_set_hl
local perl    = {}


-------------------------------------------------------------------------------
-- Settings

perl.setupHighlighting = function()
  -----------------------------------------------------------------------------
  -- Vim Syntax Groups (legacy)

  -- Keywords - Control Flow
  highlight(0, 'perlConditional',      { fg = colors.blue,       bg = 'NONE'            })  -- if, elsif, unless, given, when, default, else
  highlight(0, 'perlRepeat',           { fg = colors.blue,       bg = 'NONE'            })  -- while, for, foreach, do, until, continue
  highlight(0, 'perlLabel',            { fg = colors.blue,       bg = 'NONE'            })  -- Labels
  highlight(0, 'perlStatementControl', { fg = colors.blue,       bg = 'NONE'            })  -- return, last, next, redo, goto, break

  -- Keywords - Special Blocks
  highlight(0, 'perlControl',          { fg = colors.blue,       bg = 'NONE'            })  -- BEGIN, CHECK, INIT, END, UNITCHECK

  -- Keywords - Storage/Declaration
  highlight(0, 'perlStatementStorage', { fg = colors.blue,       bg = 'NONE'            })  -- my, our, local, state

  -- Keywords - Operators
  highlight(0, 'perlOperator',         { fg = colors.blue,       bg = 'NONE'            })  -- defined, undef, eq, ne, lt, le, gt, ge, cmp, not, and, or, xor, bless, ref

  -- Keywords - Include
  highlight(0, 'perlStatementInclude', { fg = colors.pink,       bg = 'NONE'            })  -- require, import, unimport, use, no
  highlight(0, 'perlInclude',          { fg = colors.pink,       bg = 'NONE'            })  -- use, require, no

  -- Keywords - Package/Module
  highlight(0, 'perlPackageDecl',      { fg = colors.blue,       bg = 'NONE'            })  -- package
  highlight(0, 'perlPackageRef',       { fg = colors.turquoise,  bg = 'NONE'            })  -- Package references

  -- Keywords - Subroutines
  highlight(0, 'perlSubDeclaration',   { fg = colors.blue,       bg = 'NONE'            })  -- sub
  highlight(0, 'perlSubName',          { fg = colors.orange,     bg = 'NONE'            })  -- Subroutine name
  highlight(0, 'perlSubPrototype',     { fg = colors.turquoise,  bg = 'NONE'            })  -- Prototype ($@%)
  highlight(0, 'perlSubAttributes',    { fg = colors.pink,       bg = 'NONE'            })  -- :lvalue, :method, etc.
  highlight(0, 'perlFunction',         { fg = colors.orange,     bg = 'NONE'            })  -- Function names
  highlight(0, 'perlMethod',           { fg = colors.orange,     bg = 'NONE'            })  -- Method calls

  -- Built-in Functions - Scalar
  highlight(0, 'perlStatementScalar',  { fg = colors.orange,     bg = 'NONE'            })  -- chomp, chop, chr, crypt, index, rindex, lc, lcfirst, length, ord, pack, sprintf, substr, fc, uc, ucfirst

  -- Built-in Functions - Regexp
  highlight(0, 'perlStatementRegexp',  { fg = colors.orange,     bg = 'NONE'            })  -- pos, quotemeta, split, study

  -- Built-in Functions - Numeric
  highlight(0, 'perlStatementNumeric', { fg = colors.orange,     bg = 'NONE'            })  -- abs, atan2, cos, exp, hex, int, log, oct, rand, sin, sqrt, srand

  -- Built-in Functions - List
  highlight(0, 'perlStatementList',    { fg = colors.orange,     bg = 'NONE'            })  -- splice, unshift, shift, push, pop, join, reverse, grep, map, sort, unpack

  -- Built-in Functions - Hash
  highlight(0, 'perlStatementHash',    { fg = colors.orange,     bg = 'NONE'            })  -- delete, each, exists, keys, values

  -- Built-in Functions - Time
  highlight(0, 'perlStatementTime',    { fg = colors.orange,     bg = 'NONE'            })  -- gmtime, localtime, time

  -- Built-in Functions - I/O & File
  highlight(0, 'perlStatementFiledesc',{ fg = colors.orange,     bg = 'NONE'            })  -- binmode, close, closedir, eof, fileno, getc, lstat, printf, print, read, readdir, readline, readpipe, rewinddir, say, select, stat, telldir, tell, write, fcntl, flock, ioctl, open, opendir, seek, seekdir, sysopen, sysread, sysseek, syswrite, truncate
  highlight(0, 'perlStatementFiles',   { fg = colors.orange,     bg = 'NONE'            })  -- chdir, chmod, chown, chroot, glob, link, mkdir, readlink, rename, rmdir, symlink, umask, unlink, utime
  highlight(0, 'perlStatementIOfunc',  { fg = colors.orange,     bg = 'NONE'            })  -- syscall, dbmopen, dbmclose

  -- Built-in Functions - Process
  highlight(0, 'perlStatementProc',    { fg = colors.orange,     bg = 'NONE'            })  -- alarm, exec, fork, getpgrp, getppid, getpriority, kill, pipe, setpgrp, setpriority, sleep, system, times, wait, waitpid

  -- Built-in Functions - Flow
  highlight(0, 'perlStatementFlow',    { fg = colors.orange,     bg = 'NONE'            })  -- caller, die, dump, eval, exit, wantarray, evalbytes

  -- Built-in Functions - Socket
  highlight(0, 'perlStatementSocket',  { fg = colors.orange,     bg = 'NONE'            })  -- accept, bind, connect, getpeername, getsockname, getsockopt, listen, recv, send, setsockopt, shutdown, socket, socketpair

  -- Built-in Functions - Network
  highlight(0, 'perlStatementNetwork', { fg = colors.orange,     bg = 'NONE'            })  -- endhostent, endnetent, endprotoent, endservent, gethostbyaddr, gethostbyname, getnetbyaddr, getnetbyname, getprotobyname, getprotobynumber, getservbyname, getservbyport, gethostent, getnetent, getprotoent, getservent, sethostent, setnetent, setprotoent, setservent

  -- Built-in Functions - IPC
  highlight(0, 'perlStatementIPC',     { fg = colors.orange,     bg = 'NONE'            })  -- msgctl, msgget, msgrcv, msgsnd, semctl, semget, semop, shmctl, shmget, shmread, shmwrite

  -- Built-in Functions - Password
  highlight(0, 'perlStatementPword',   { fg = colors.orange,     bg = 'NONE'            })  -- getpwuid, getpwnam, getgrgid, getgrnam, getlogin, endpwent, endgrent, setpwent, setgrent

  -- Built-in Functions - Misc
  highlight(0, 'perlStatementMisc',    { fg = colors.orange,     bg = 'NONE'            })  -- warn, format, formline, reset, scalar, prototype, lock, tied, untie
  highlight(0, 'perlStatementVector',  { fg = colors.orange,     bg = 'NONE'            })  -- vec
  highlight(0, 'perlStatementIndirObj',{ fg = colors.orange,     bg = 'NONE'            })  -- map, grep, sort, printf, say, system, exec (with indirect object)

  -- Variables - Sigils
  highlight(0, 'perlVarPlain',         { fg = colors.purple,     bg = 'NONE'            })  -- $scalar
  highlight(0, 'perlVarPlain2',        { fg = colors.purple,     bg = 'NONE'            })  -- @array, %hash
  highlight(0, 'perlVarNotInMatches',  { fg = colors.purple,     bg = 'NONE'            })  -- Variables not in regex matches
  highlight(0, 'perlVarSlash',         { fg = colors.purple,     bg = 'NONE'            })  -- Variables in regex
  highlight(0, 'perlVarBlock',         { fg = colors.purple,     bg = 'NONE'            })  -- ${...} dereference

  -- Variables - Special/Built-in
  highlight(0, 'perlVarSimpleMember',  { fg = colors.purple,     bg = 'NONE'            })  -- Hash/array members
  highlight(0, 'perlVarSimpleMemberName', { fg = colors.purple,  bg = 'NONE'            })  -- Member names
  highlight(0, 'perlSpecialString',    { fg = colors.pink,       bg = 'NONE'            })  -- Special variables like $_, @_, %ENV
  highlight(0, 'perlSpecialMatch',     { fg = colors.pink,       bg = 'NONE'            })  -- $&, $`, $', $1, $2, etc.
  highlight(0, 'perlSpecialBEOM',      { fg = colors.pink,       bg = 'NONE'            })  -- $^, $~, etc.

  -- Variables - Identifiers
  highlight(0, 'perlIdentifier',       { fg = colors.purple,     bg = 'NONE'            })  -- General identifiers

  -- Types/Classes
  highlight(0, 'perlType',             { fg = colors.turquoise,  bg = 'NONE'            })  -- Type names
  highlight(0, 'perlPackage',          { fg = colors.turquoise,  bg = 'NONE'            })  -- Package names
  highlight(0, 'perlFunctionName',     { fg = colors.orange,     bg = 'NONE'            })  -- Function names

  -- Strings
  highlight(0, 'perlString',           { fg = colors.redLight,   bg = 'NONE'            })  -- "strings" and 'strings'
  highlight(0, 'perlStringUnexpanded', { fg = colors.redLight,   bg = 'NONE'            })  -- 'literal strings' (no interpolation)
  highlight(0, 'perlQQ',               { fg = colors.redLight,   bg = 'NONE'            })  -- qq{...}
  highlight(0, 'perlQ',                { fg = colors.redLight,   bg = 'NONE'            })  -- q{...}
  highlight(0, 'perlHereDoc',          { fg = colors.redLight,   bg = 'NONE'            })  -- <<EOF heredocs
  highlight(0, 'perlIndentedHereDoc',  { fg = colors.redLight,   bg = 'NONE'            })  -- <<~EOF indented heredocs

  -- String Escapes
  highlight(0, 'perlSpecialStringU',   { fg = colors.pink,       bg = 'NONE'            })  -- Unicode escapes
  highlight(0, 'perlSpecialStringU2',  { fg = colors.pink,       bg = 'NONE'            })  -- Named Unicode escapes

  -- Numbers
  highlight(0, 'perlNumber',           { fg = colors.greenLight, bg = 'NONE'            })  -- Numbers
  highlight(0, 'perlFloat',            { fg = colors.greenLight, bg = 'NONE'            })  -- Floating-point

  -- Regular Expressions
  highlight(0, 'perlMatch',            { fg = colors.redLight,   bg = 'NONE'            })  -- m// match
  highlight(0, 'perlMatchStartEnd',    { fg = colors.pink,       bg = 'NONE'            })  -- Regex delimiters
  highlight(0, 'perlSubstitution',     { fg = colors.redLight,   bg = 'NONE'            })  -- s/// substitution
  highlight(0, 'perlSubstitutionSQ',   { fg = colors.redLight,   bg = 'NONE'            })  -- s''' substitution (no interpolation)
  highlight(0, 'perlSubstitutionGQQ',  { fg = colors.redLight,   bg = 'NONE'            })  -- s{}{} substitution
  highlight(0, 'perlSubstitutionSlash',{ fg = colors.pink,       bg = 'NONE'            })  -- s/// delimiter
  highlight(0, 'perlTranslation',      { fg = colors.redLight,   bg = 'NONE'            })  -- tr/// and y///
  highlight(0, 'perlTranslationGQ',    { fg = colors.redLight,   bg = 'NONE'            })  -- tr{}{} and y{}{}

  -- Regex Components
  highlight(0, 'perlSpecialAscii',     { fg = colors.pink,       bg = 'NONE'            })  -- \n, \t, \r, etc.
  highlight(0, 'perlSpecialDollar',    { fg = colors.pink,       bg = 'NONE'            })  -- $ in regex
  highlight(0, 'perlBrackets',         { fg = colors.white,      bg = 'NONE'            })  -- Character classes [...]

  -- Operators
  highlight(0, 'perlArrow',            { fg = colors.white,      bg = 'NONE'            })  -- -> arrow operator
  highlight(0, 'perlFatComma',         { fg = colors.white,      bg = 'NONE'            })  -- => fat comma
  highlight(0, 'perlRange',            { fg = colors.white,      bg = 'NONE'            })  -- .. and ... range
  highlight(0, 'perlOperatorSymbol',   { fg = colors.white,      bg = 'NONE'            })  -- +, -, *, /, %, etc.

  -- Quote-like Operators
  highlight(0, 'perlQW',               { fg = colors.redLight,   bg = 'NONE'            })  -- qw(...) word list
  highlight(0, 'perlQR',               { fg = colors.redLight,   bg = 'NONE'            })  -- qr{} compiled regex
  highlight(0, 'perlQX',               { fg = colors.redLight,   bg = 'NONE'            })  -- qx{} or `` backticks

  -- POD Documentation
  highlight(0, 'perlPOD',              { fg = colors.red,        bg = 'NONE'            })  -- POD documentation
  highlight(0, 'perlPODCommand',       { fg = colors.green,      bg = 'NONE'            })  -- =head1, =over, =item, etc.
  highlight(0, 'perlPODFormat',        { fg = colors.green,      bg = 'NONE'            })  -- B<>, I<>, C<>, L<>, etc.
  highlight(0, 'podCommand',           { fg = colors.green,      bg = 'NONE'            })  -- POD commands
  highlight(0, 'podFormat',            { fg = colors.green,      bg = 'NONE'            })  -- POD formatting
  highlight(0, 'podVerbatimLine',      { fg = colors.redLight,   bg = 'NONE'            })  -- Verbatim code blocks
  highlight(0, 'podSpecial',           { fg = colors.pink,       bg = 'NONE'            })  -- Special POD elements

  -- Comments
  highlight(0, 'perlComment',          { fg = colors.red,        bg = 'NONE'            })  -- # comments
  highlight(0, 'perlTodo',             { fg = colors.red,        bg = 'NONE', bold = true })  -- TODO, FIXME, XXX

  -- Shebang
  highlight(0, 'perlSharpBang',        { fg = colors.pink,       bg = 'NONE'            })  -- #!/usr/bin/perl

  -- Data Section
  highlight(0, 'perlDATA',             { fg = colors.red,        bg = 'NONE'            })  -- __DATA__ and __END__ sections

  -- Filehandles
  highlight(0, 'perlFiledescRead',     { fg = colors.turquoise,  bg = 'NONE'            })  -- <FILEHANDLE>
  highlight(0, 'perlFiledescStatement',{ fg = colors.turquoise,  bg = 'NONE'            })  -- STDIN, STDOUT, STDERR, ARGV, DATA

  -- Format
  highlight(0, 'perlFormat',           { fg = colors.blue,       bg = 'NONE'            })  -- format definitions
  highlight(0, 'perlFormatName',       { fg = colors.orange,     bg = 'NONE'            })  -- format name
  highlight(0, 'perlFormatField',      { fg = colors.purple,     bg = 'NONE'            })  -- format fields

  -- Moose/Moo OOP
  highlight(0, 'perlMooseKeyword',     { fg = colors.blue,       bg = 'NONE'            })  -- has, extends, with, before, after, around, override, augment
  highlight(0, 'perlMooseAttribute',   { fg = colors.purple,     bg = 'NONE'            })  -- is, isa, required, default, lazy, builder, etc.

  -- Error/Special
  highlight(0, 'perlNotEmptyLine',     { fg = colors.red,        bg = 'NONE'            })  -- Error lines
  highlight(0, 'perlElseIfError',      { fg = colors.red,        bg = 'NONE'            })  -- elsif errors


  -----------------------------------------------------------------------------
  -- Treesitter Groups (@xxx.perl)

  -- Variables
  highlight(0, '@variable.perl',              { fg = colors.purple,    bg = 'NONE' })  -- Variables
  highlight(0, '@variable.builtin.perl',      { fg = colors.pink,      bg = 'NONE' })  -- $_, @_, %ENV, STDIN, STDOUT, STDERR, ARGV
  highlight(0, '@variable.parameter.perl',    { fg = colors.purple,    bg = 'NONE' })  -- Subroutine parameters
  highlight(0, '@variable.member.perl',       { fg = colors.purple,    bg = 'NONE' })  -- Hash keys, array indices

  -- Constants
  highlight(0, '@constant.perl',              { fg = colors.purple,    bg = 'NONE' })  -- Constants
  highlight(0, '@constant.builtin.perl',      { fg = colors.blue,      bg = 'NONE' })  -- __FILE__, __LINE__, __PACKAGE__

  -- Functions
  highlight(0, '@function.perl',              { fg = colors.orange,    bg = 'NONE' })  -- Subroutine definitions
  highlight(0, '@function.call.perl',         { fg = colors.orange,    bg = 'NONE' })  -- Subroutine calls
  highlight(0, '@function.builtin.perl',      { fg = colors.orange,    bg = 'NONE' })  -- Built-in functions (map, grep, sort, print, etc.)
  highlight(0, '@function.method.perl',       { fg = colors.orange,    bg = 'NONE' })  -- Method definitions
  highlight(0, '@function.method.call.perl',  { fg = colors.orange,    bg = 'NONE' })  -- Method calls

  -- Types
  highlight(0, '@type.perl',                  { fg = colors.turquoise, bg = 'NONE' })  -- Package/class names
  highlight(0, '@type.builtin.perl',          { fg = colors.turquoise, bg = 'NONE' })  -- Built-in types

  -- Modules
  highlight(0, '@module.perl',                { fg = colors.turquoise, bg = 'NONE' })  -- Package names

  -- Attributes
  highlight(0, '@attribute.perl',             { fg = colors.pink,      bg = 'NONE' })  -- Subroutine attributes (:lvalue, :method)

  -- Labels
  highlight(0, '@label.perl',                 { fg = colors.blue,      bg = 'NONE' })  -- Labels

  -- Keywords
  highlight(0, '@keyword.perl',               { fg = colors.blue,      bg = 'NONE' })  -- General keywords
  highlight(0, '@keyword.function.perl',      { fg = colors.blue,      bg = 'NONE' })  -- sub
  highlight(0, '@keyword.operator.perl',      { fg = colors.blue,      bg = 'NONE' })  -- and, or, not, xor, eq, ne, lt, le, gt, ge, cmp, isa
  highlight(0, '@keyword.return.perl',        { fg = colors.blue,      bg = 'NONE' })  -- return
  highlight(0, '@keyword.repeat.perl',        { fg = colors.blue,      bg = 'NONE' })  -- while, for, foreach, until, continue
  highlight(0, '@keyword.conditional.perl',   { fg = colors.blue,      bg = 'NONE' })  -- if, elsif, unless, else, given, when, default
  highlight(0, '@keyword.exception.perl',     { fg = colors.blue,      bg = 'NONE' })  -- try, catch, finally
  highlight(0, '@keyword.import.perl',        { fg = colors.pink,      bg = 'NONE' })  -- use, no, require, package
  highlight(0, '@keyword.coroutine.perl',     { fg = colors.blue,      bg = 'NONE' })  -- async, await
  highlight(0, '@keyword.directive.perl',     { fg = colors.pink,      bg = 'NONE' })  -- Shebang and pragmas

  -- Strings
  highlight(0, '@string.perl',                { fg = colors.redLight,  bg = 'NONE' })  -- Strings
  highlight(0, '@string.regexp.perl',         { fg = colors.redLight,  bg = 'NONE' })  -- Regular expressions
  highlight(0, '@string.escape.perl',         { fg = colors.pink,      bg = 'NONE' })  -- Escape sequences
  highlight(0, '@string.special.perl',        { fg = colors.redLight,  bg = 'NONE' })  -- Autoquoted barewords
  highlight(0, '@string.special.symbol.perl', { fg = colors.redLight,  bg = 'NONE' })  -- __DATA__, __END__
  highlight(0, '@character.perl',             { fg = colors.redLight,  bg = 'NONE' })  -- Character literals
  highlight(0, '@character.special.perl',     { fg = colors.pink,      bg = 'NONE' })  -- Special characters

  -- Numbers
  highlight(0, '@number.perl',                { fg = colors.greenLight, bg = 'NONE' })  -- Integers
  highlight(0, '@number.float.perl',          { fg = colors.greenLight, bg = 'NONE' })  -- Floats

  -- Booleans
  highlight(0, '@boolean.perl',               { fg = colors.blue,      bg = 'NONE' })  -- (Perl doesn't have true/false keywords)

  -- Comments
  highlight(0, '@comment.perl',               { fg = colors.red,       bg = 'NONE' })  -- Comments
  highlight(0, '@comment.documentation.perl', { fg = colors.red,       bg = 'NONE' })  -- POD documentation

  -- Operators and Punctuation
  highlight(0, '@operator.perl',              { fg = colors.white,     bg = 'NONE' })  -- Operators
  highlight(0, '@punctuation.bracket.perl',   { fg = colors.white,     bg = 'NONE' })  -- (), [], {}
  highlight(0, '@punctuation.delimiter.perl', { fg = colors.white,     bg = 'NONE' })  -- , ; : => ->
  highlight(0, '@punctuation.special.perl',   { fg = colors.pink,      bg = 'NONE' })  -- $ @ % & * in variable contexts


  -----------------------------------------------------------------------------
  -- LSP Semantic Tokens (@lsp.type.xxx.perl)

  highlight(0, '@lsp.type.variable.perl',      { fg = colors.purple,    bg = 'NONE' })  -- Variables
  highlight(0, '@lsp.type.parameter.perl',     { fg = colors.purple,    bg = 'NONE' })  -- Parameters
  highlight(0, '@lsp.type.property.perl',      { fg = colors.purple,    bg = 'NONE' })  -- Properties (hash keys)
  highlight(0, '@lsp.type.function.perl',      { fg = colors.orange,    bg = 'NONE' })  -- Subroutines
  highlight(0, '@lsp.type.method.perl',        { fg = colors.orange,    bg = 'NONE' })  -- Methods
  highlight(0, '@lsp.type.type.perl',          { fg = colors.turquoise, bg = 'NONE' })  -- Types
  highlight(0, '@lsp.type.class.perl',         { fg = colors.turquoise, bg = 'NONE' })  -- Classes (packages)
  highlight(0, '@lsp.type.namespace.perl',     { fg = colors.turquoise, bg = 'NONE' })  -- Namespaces
  highlight(0, '@lsp.type.keyword.perl',       { fg = colors.blue,      bg = 'NONE' })  -- Keywords
  highlight(0, '@lsp.type.modifier.perl',      { fg = colors.blue,      bg = 'NONE' })  -- Modifiers
  highlight(0, '@lsp.type.operator.perl',      { fg = colors.white,     bg = 'NONE' })  -- Operators
  highlight(0, '@lsp.type.string.perl',        { fg = colors.redLight,  bg = 'NONE' })  -- Strings
  highlight(0, '@lsp.type.regexp.perl',        { fg = colors.redLight,  bg = 'NONE' })  -- Regular expressions
  highlight(0, '@lsp.type.number.perl',        { fg = colors.greenLight, bg = 'NONE' }) -- Numbers
  highlight(0, '@lsp.type.comment.perl',       { fg = colors.red,       bg = 'NONE' })  -- Comments

  -- LSP Modifiers
  highlight(0, '@lsp.typemod.variable.readonly.perl',    { fg = colors.purple,    bg = 'NONE' })  -- Constants
  highlight(0, '@lsp.typemod.function.declaration.perl', { fg = colors.orange,    bg = 'NONE' })  -- Subroutine declarations
  highlight(0, '@lsp.typemod.function.defaultLibrary.perl', { fg = colors.orange, bg = 'NONE' })  -- Built-in functions
  highlight(0, '@lsp.typemod.class.declaration.perl',    { fg = colors.turquoise, bg = 'NONE' })  -- Package declarations
end

return perl

