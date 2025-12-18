-------------------------------------------------------------------------------
-- Scratch
-- Highlighting for Scratch-related text formats:
-- - Scratchblocks syntax (text representation of Scratch blocks)
-- - SB3/SB2 project.json (JSON structure)
-- Note: .sb3/.sb2 files are ZIP archives containing JSON
-------------------------------------------------------------------------------

local colors    = require('mannydark.palette')
local highlight = vim.api.nvim_set_hl
local scratch   = {}


-------------------------------------------------------------------------------
-- Settings

scratch.setupHighlighting = function()
  -----------------------------------------------------------------------------
  -- Scratchblocks Syntax
  -- Text representation used on Scratch Wiki and Forums

  -- Block Categories (matching Scratch's color scheme conceptually)
  -- Motion blocks (blue in Scratch)
  highlight(0, 'scratchblocksMotion',         { fg = colors.blue,       bg = 'NONE' })  -- Motion blocks
  highlight(0, 'scratchblocksMove',           { fg = colors.blue,       bg = 'NONE' })  -- move () steps
  highlight(0, 'scratchblocksTurn',           { fg = colors.blue,       bg = 'NONE' })  -- turn cw/ccw () degrees
  highlight(0, 'scratchblocksGoto',           { fg = colors.blue,       bg = 'NONE' })  -- go to (), go to x: () y: ()
  highlight(0, 'scratchblocksGlide',          { fg = colors.blue,       bg = 'NONE' })  -- glide () secs to ()
  highlight(0, 'scratchblocksPoint',          { fg = colors.blue,       bg = 'NONE' })  -- point in direction (), point towards ()
  highlight(0, 'scratchblocksChange',         { fg = colors.blue,       bg = 'NONE' })  -- change x by (), change y by ()
  highlight(0, 'scratchblocksSet',            { fg = colors.blue,       bg = 'NONE' })  -- set x to (), set y to ()
  highlight(0, 'scratchblocksBounce',         { fg = colors.blue,       bg = 'NONE' })  -- if on edge, bounce
  highlight(0, 'scratchblocksRotation',       { fg = colors.blue,       bg = 'NONE' })  -- set rotation style []

  -- Looks blocks (purple in Scratch)
  highlight(0, 'scratchblocksLooks',          { fg = colors.purple,     bg = 'NONE' })  -- Looks blocks
  highlight(0, 'scratchblocksSay',            { fg = colors.purple,     bg = 'NONE' })  -- say [], say [] for () seconds
  highlight(0, 'scratchblocksThink',          { fg = colors.purple,     bg = 'NONE' })  -- think [], think [] for () seconds
  highlight(0, 'scratchblocksCostume',        { fg = colors.purple,     bg = 'NONE' })  -- switch costume to (), next costume
  highlight(0, 'scratchblocksBackdrop',       { fg = colors.purple,     bg = 'NONE' })  -- switch backdrop to (), next backdrop
  highlight(0, 'scratchblocksSize',           { fg = colors.purple,     bg = 'NONE' })  -- set size to ()%, change size by ()
  highlight(0, 'scratchblocksEffect',         { fg = colors.purple,     bg = 'NONE' })  -- set [] effect to (), change [] effect by ()
  highlight(0, 'scratchblocksShow',           { fg = colors.purple,     bg = 'NONE' })  -- show, hide
  highlight(0, 'scratchblocksLayer',          { fg = colors.purple,     bg = 'NONE' })  -- go to [] layer, go [] () layers

  -- Sound blocks (magenta/pink in Scratch)
  highlight(0, 'scratchblocksSound',          { fg = colors.pink,       bg = 'NONE' })  -- Sound blocks
  highlight(0, 'scratchblocksPlay',           { fg = colors.pink,       bg = 'NONE' })  -- play sound (), play sound () until done
  highlight(0, 'scratchblocksStop',           { fg = colors.pink,       bg = 'NONE' })  -- stop all sounds
  highlight(0, 'scratchblocksVolume',         { fg = colors.pink,       bg = 'NONE' })  -- set volume to ()%, change volume by ()
  highlight(0, 'scratchblocksPitch',          { fg = colors.pink,       bg = 'NONE' })  -- set pitch effect to ()

  -- Events blocks (yellow/orange in Scratch)
  highlight(0, 'scratchblocksEvents',         { fg = colors.orange,     bg = 'NONE' })  -- Events blocks
  highlight(0, 'scratchblocksWhenFlag',       { fg = colors.orange,     bg = 'NONE' })  -- when green flag clicked
  highlight(0, 'scratchblocksWhenKey',        { fg = colors.orange,     bg = 'NONE' })  -- when [] key pressed
  highlight(0, 'scratchblocksWhenClicked',    { fg = colors.orange,     bg = 'NONE' })  -- when this sprite clicked
  highlight(0, 'scratchblocksWhenBackdrop',   { fg = colors.orange,     bg = 'NONE' })  -- when backdrop switches to []
  highlight(0, 'scratchblocksWhenGreater',    { fg = colors.orange,     bg = 'NONE' })  -- when [] > ()
  highlight(0, 'scratchblocksWhenReceive',    { fg = colors.orange,     bg = 'NONE' })  -- when I receive []
  highlight(0, 'scratchblocksBroadcast',      { fg = colors.orange,     bg = 'NONE' })  -- broadcast [], broadcast [] and wait

  -- Control blocks (orange/gold in Scratch)
  highlight(0, 'scratchblocksControl',        { fg = colors.orange,     bg = 'NONE' })  -- Control blocks
  highlight(0, 'scratchblocksWait',           { fg = colors.orange,     bg = 'NONE' })  -- wait () seconds, wait until <>
  highlight(0, 'scratchblocksRepeat',         { fg = colors.orange,     bg = 'NONE' })  -- repeat ()
  highlight(0, 'scratchblocksForever',        { fg = colors.orange,     bg = 'NONE' })  -- forever
  highlight(0, 'scratchblocksIf',             { fg = colors.orange,     bg = 'NONE' })  -- if <> then
  highlight(0, 'scratchblocksIfElse',         { fg = colors.orange,     bg = 'NONE' })  -- if <> then ... else
  highlight(0, 'scratchblocksRepeatUntil',    { fg = colors.orange,     bg = 'NONE' })  -- repeat until <>
  highlight(0, 'scratchblocksStopAll',        { fg = colors.orange,     bg = 'NONE' })  -- stop [all v]
  highlight(0, 'scratchblocksClone',          { fg = colors.orange,     bg = 'NONE' })  -- create clone of [], when I start as a clone
  highlight(0, 'scratchblocksDelete',         { fg = colors.orange,     bg = 'NONE' })  -- delete this clone

  -- Sensing blocks (cyan/light blue in Scratch)
  highlight(0, 'scratchblocksSensing',        { fg = colors.turquoise,  bg = 'NONE' })  -- Sensing blocks
  highlight(0, 'scratchblocksTouching',       { fg = colors.turquoise,  bg = 'NONE' })  -- <touching ()?>
  highlight(0, 'scratchblocksTouchingColor',  { fg = colors.turquoise,  bg = 'NONE' })  -- <touching color []?>
  highlight(0, 'scratchblocksColorTouching',  { fg = colors.turquoise,  bg = 'NONE' })  -- <color [] is touching []?>
  highlight(0, 'scratchblocksDistance',       { fg = colors.turquoise,  bg = 'NONE' })  -- (distance to ())
  highlight(0, 'scratchblocksAsk',            { fg = colors.turquoise,  bg = 'NONE' })  -- ask [] and wait
  highlight(0, 'scratchblocksAnswer',         { fg = colors.turquoise,  bg = 'NONE' })  -- (answer)
  highlight(0, 'scratchblocksKeyPressed',     { fg = colors.turquoise,  bg = 'NONE' })  -- <key () pressed?>
  highlight(0, 'scratchblocksMouseDown',      { fg = colors.turquoise,  bg = 'NONE' })  -- <mouse down?>
  highlight(0, 'scratchblocksMouseXY',        { fg = colors.turquoise,  bg = 'NONE' })  -- (mouse x), (mouse y)
  highlight(0, 'scratchblocksDraggable',      { fg = colors.turquoise,  bg = 'NONE' })  -- set drag mode []
  highlight(0, 'scratchblocksLoudness',       { fg = colors.turquoise,  bg = 'NONE' })  -- (loudness)
  highlight(0, 'scratchblocksTimer',          { fg = colors.turquoise,  bg = 'NONE' })  -- (timer), reset timer
  highlight(0, 'scratchblocksOf',             { fg = colors.turquoise,  bg = 'NONE' })  -- ([] of ())
  highlight(0, 'scratchblocksCurrent',        { fg = colors.turquoise,  bg = 'NONE' })  -- (current [])
  highlight(0, 'scratchblocksDaysSince',      { fg = colors.turquoise,  bg = 'NONE' })  -- (days since 2000)
  highlight(0, 'scratchblocksUsername',       { fg = colors.turquoise,  bg = 'NONE' })  -- (username)

  -- Operators blocks (green in Scratch)
  highlight(0, 'scratchblocksOperators',      { fg = colors.green,      bg = 'NONE' })  -- Operators blocks
  highlight(0, 'scratchblocksAdd',            { fg = colors.green,      bg = 'NONE' })  -- (() + ())
  highlight(0, 'scratchblocksSubtract',       { fg = colors.green,      bg = 'NONE' })  -- (() - ())
  highlight(0, 'scratchblocksMultiply',       { fg = colors.green,      bg = 'NONE' })  -- (() * ())
  highlight(0, 'scratchblocksDivide',         { fg = colors.green,      bg = 'NONE' })  -- (() / ())
  highlight(0, 'scratchblocksRandom',         { fg = colors.green,      bg = 'NONE' })  -- (pick random () to ())
  highlight(0, 'scratchblocksGreater',        { fg = colors.green,      bg = 'NONE' })  -- <() > ()>
  highlight(0, 'scratchblocksLess',           { fg = colors.green,      bg = 'NONE' })  -- <() < ()>
  highlight(0, 'scratchblocksEqual',          { fg = colors.green,      bg = 'NONE' })  -- <() = ()>
  highlight(0, 'scratchblocksAnd',            { fg = colors.green,      bg = 'NONE' })  -- <<> and <>>
  highlight(0, 'scratchblocksOr',             { fg = colors.green,      bg = 'NONE' })  -- <<> or <>>
  highlight(0, 'scratchblocksNot',            { fg = colors.green,      bg = 'NONE' })  -- <not <>>
  highlight(0, 'scratchblocksJoin',           { fg = colors.green,      bg = 'NONE' })  -- (join [] [])
  highlight(0, 'scratchblocksLetter',         { fg = colors.green,      bg = 'NONE' })  -- (letter () of [])
  highlight(0, 'scratchblocksLength',         { fg = colors.green,      bg = 'NONE' })  -- (length of [])
  highlight(0, 'scratchblocksContains',       { fg = colors.green,      bg = 'NONE' })  -- <[] contains []?>
  highlight(0, 'scratchblocksMod',            { fg = colors.green,      bg = 'NONE' })  -- (() mod ())
  highlight(0, 'scratchblocksRound',          { fg = colors.green,      bg = 'NONE' })  -- (round ())
  highlight(0, 'scratchblocksMathOp',         { fg = colors.green,      bg = 'NONE' })  -- ([] of ()) - abs, floor, ceiling, sqrt, sin, cos, tan, etc.

  -- Variables blocks (orange in Scratch)
  highlight(0, 'scratchblocksVariables',      { fg = colors.orange,     bg = 'NONE' })  -- Variables blocks
  highlight(0, 'scratchblocksVariable',       { fg = colors.orange,     bg = 'NONE' })  -- (variable)
  highlight(0, 'scratchblocksSetVar',         { fg = colors.orange,     bg = 'NONE' })  -- set [] to ()
  highlight(0, 'scratchblocksChangeVar',      { fg = colors.orange,     bg = 'NONE' })  -- change [] by ()
  highlight(0, 'scratchblocksShowVar',        { fg = colors.orange,     bg = 'NONE' })  -- show variable [], hide variable []

  -- List blocks (orange/red in Scratch)
  highlight(0, 'scratchblocksLists',          { fg = colors.orange,     bg = 'NONE' })  -- List blocks
  highlight(0, 'scratchblocksAddToList',      { fg = colors.orange,     bg = 'NONE' })  -- add [] to [list v]
  highlight(0, 'scratchblocksDeleteOfList',   { fg = colors.orange,     bg = 'NONE' })  -- delete () of [list v]
  highlight(0, 'scratchblocksDeleteAll',      { fg = colors.orange,     bg = 'NONE' })  -- delete all of [list v]
  highlight(0, 'scratchblocksInsert',         { fg = colors.orange,     bg = 'NONE' })  -- insert [] at () of [list v]
  highlight(0, 'scratchblocksReplace',        { fg = colors.orange,     bg = 'NONE' })  -- replace item () of [list v] with []
  highlight(0, 'scratchblocksItemOf',         { fg = colors.orange,     bg = 'NONE' })  -- (item () of [list v])
  highlight(0, 'scratchblocksItemNum',        { fg = colors.orange,     bg = 'NONE' })  -- (item # of [] in [list v])
  highlight(0, 'scratchblocksLengthOfList',   { fg = colors.orange,     bg = 'NONE' })  -- (length of [list v])
  highlight(0, 'scratchblocksListContains',   { fg = colors.orange,     bg = 'NONE' })  -- <[list v] contains []?>
  highlight(0, 'scratchblocksShowList',       { fg = colors.orange,     bg = 'NONE' })  -- show list [], hide list []

  -- My Blocks / Custom blocks (pink/red in Scratch)
  highlight(0, 'scratchblocksCustom',         { fg = colors.pink,       bg = 'NONE' })  -- Custom blocks
  highlight(0, 'scratchblocksDefine',         { fg = colors.pink,       bg = 'NONE' })  -- define
  highlight(0, 'scratchblocksCustomBlock',    { fg = colors.pink,       bg = 'NONE' })  -- custom block call

  -- Pen blocks (teal/green extension)
  highlight(0, 'scratchblocksPen',            { fg = colors.turquoise,  bg = 'NONE' })  -- Pen blocks
  highlight(0, 'scratchblocksErase',          { fg = colors.turquoise,  bg = 'NONE' })  -- erase all
  highlight(0, 'scratchblocksStamp',          { fg = colors.turquoise,  bg = 'NONE' })  -- stamp
  highlight(0, 'scratchblocksPenDown',        { fg = colors.turquoise,  bg = 'NONE' })  -- pen down, pen up
  highlight(0, 'scratchblocksPenColor',       { fg = colors.turquoise,  bg = 'NONE' })  -- set pen color to []
  highlight(0, 'scratchblocksPenSize',        { fg = colors.turquoise,  bg = 'NONE' })  -- set pen size to ()

  -- Music blocks (pink extension)
  highlight(0, 'scratchblocksMusic',          { fg = colors.pink,       bg = 'NONE' })  -- Music blocks
  highlight(0, 'scratchblocksPlayDrum',       { fg = colors.pink,       bg = 'NONE' })  -- play drum () for () beats
  highlight(0, 'scratchblocksPlayNote',       { fg = colors.pink,       bg = 'NONE' })  -- play note () for () beats
  highlight(0, 'scratchblocksSetInstrument',  { fg = colors.pink,       bg = 'NONE' })  -- set instrument to ()
  highlight(0, 'scratchblocksSetTempo',       { fg = colors.pink,       bg = 'NONE' })  -- set tempo to ()

  -- Video Sensing blocks (blue extension)
  highlight(0, 'scratchblocksVideo',          { fg = colors.blue,       bg = 'NONE' })  -- Video Sensing blocks

  -- Text to Speech blocks (purple extension)
  highlight(0, 'scratchblocksTTS',            { fg = colors.purple,     bg = 'NONE' })  -- Text to Speech blocks
  highlight(0, 'scratchblocksSpeak',          { fg = colors.purple,     bg = 'NONE' })  -- speak []
  highlight(0, 'scratchblocksSetVoice',       { fg = colors.purple,     bg = 'NONE' })  -- set voice to ()

  -- Translate blocks (green extension)
  highlight(0, 'scratchblocksTranslate',      { fg = colors.green,      bg = 'NONE' })  -- Translate blocks


  -----------------------------------------------------------------------------
  -- Scratchblocks Syntax Elements

  -- Arguments
  highlight(0, 'scratchblocksNumber',         { fg = colors.greenLight, bg = 'NONE' })  -- (10) - number arguments
  highlight(0, 'scratchblocksString',         { fg = colors.redLight,   bg = 'NONE' })  -- [text] - string arguments
  highlight(0, 'scratchblocksBoolean',        { fg = colors.turquoise,  bg = 'NONE' })  -- <condition> - boolean arguments
  highlight(0, 'scratchblocksReporter',       { fg = colors.orange,     bg = 'NONE' })  -- (reporter) - reporter blocks

  -- Dropdown menus
  highlight(0, 'scratchblocksDropdown',       { fg = colors.white,      bg = 'NONE' })  -- [selection v]
  highlight(0, 'scratchblocksDropdownArrow',  { fg = colors.white,      bg = 'NONE' })  -- v in dropdown

  -- Color inputs
  highlight(0, 'scratchblocksColor',          { fg = colors.purple,     bg = 'NONE' })  -- [#1540bf] - color hex

  -- Comments
  highlight(0, 'scratchblocksComment',        { fg = colors.red,        bg = 'NONE' })  -- // comment

  -- Special syntax
  highlight(0, 'scratchblocksCategory',       { fg = colors.pink,       bg = 'NONE' })  -- :: motion, :: looks, etc.
  highlight(0, 'scratchblocksShape',          { fg = colors.pink,       bg = 'NONE' })  -- :: hat, :: stack, :: reporter
  highlight(0, 'scratchblocksColorOverride',  { fg = colors.pink,       bg = 'NONE' })  -- :: #228b22

  -- Icons
  highlight(0, 'scratchblocksIcon',           { fg = colors.green,      bg = 'NONE' })  -- @greenFlag, @stopSign, @turnRight, @turnLeft

  -- Block structure
  highlight(0, 'scratchblocksEnd',            { fg = colors.orange,     bg = 'NONE' })  -- end (closes C blocks)

  -- Delimiters
  highlight(0, 'scratchblocksParens',         { fg = colors.white,      bg = 'NONE' })  -- ( )
  highlight(0, 'scratchblocksBrackets',       { fg = colors.white,      bg = 'NONE' })  -- [ ]
  highlight(0, 'scratchblocksAngleBrackets',  { fg = colors.white,      bg = 'NONE' })  -- < >


  -----------------------------------------------------------------------------
  -- SB3/SB2 Project JSON Format

  -- Top-level keys
  highlight(0, 'scratchJsonTargets',          { fg = colors.turquoise,  bg = 'NONE' })  -- "targets"
  highlight(0, 'scratchJsonMonitors',         { fg = colors.turquoise,  bg = 'NONE' })  -- "monitors"
  highlight(0, 'scratchJsonExtensions',       { fg = colors.turquoise,  bg = 'NONE' })  -- "extensions"
  highlight(0, 'scratchJsonMeta',             { fg = colors.turquoise,  bg = 'NONE' })  -- "meta"

  -- Target properties
  highlight(0, 'scratchJsonIsStage',          { fg = colors.purple,     bg = 'NONE' })  -- "isStage"
  highlight(0, 'scratchJsonName',             { fg = colors.purple,     bg = 'NONE' })  -- "name"
  highlight(0, 'scratchJsonVariables',        { fg = colors.purple,     bg = 'NONE' })  -- "variables"
  highlight(0, 'scratchJsonLists',            { fg = colors.purple,     bg = 'NONE' })  -- "lists"
  highlight(0, 'scratchJsonBroadcasts',       { fg = colors.purple,     bg = 'NONE' })  -- "broadcasts"
  highlight(0, 'scratchJsonBlocks',           { fg = colors.purple,     bg = 'NONE' })  -- "blocks"
  highlight(0, 'scratchJsonComments',         { fg = colors.purple,     bg = 'NONE' })  -- "comments"
  highlight(0, 'scratchJsonCostumes',         { fg = colors.purple,     bg = 'NONE' })  -- "costumes"
  highlight(0, 'scratchJsonSounds',           { fg = colors.purple,     bg = 'NONE' })  -- "sounds"
  highlight(0, 'scratchJsonCurrentCostume',   { fg = colors.purple,     bg = 'NONE' })  -- "currentCostume"
  highlight(0, 'scratchJsonLayerOrder',       { fg = colors.purple,     bg = 'NONE' })  -- "layerOrder"
  highlight(0, 'scratchJsonVolume',           { fg = colors.purple,     bg = 'NONE' })  -- "volume"

  -- Sprite-specific properties
  highlight(0, 'scratchJsonVisible',          { fg = colors.purple,     bg = 'NONE' })  -- "visible"
  highlight(0, 'scratchJsonX',                { fg = colors.purple,     bg = 'NONE' })  -- "x"
  highlight(0, 'scratchJsonY',                { fg = colors.purple,     bg = 'NONE' })  -- "y"
  highlight(0, 'scratchJsonSize',             { fg = colors.purple,     bg = 'NONE' })  -- "size"
  highlight(0, 'scratchJsonDirection',        { fg = colors.purple,     bg = 'NONE' })  -- "direction"
  highlight(0, 'scratchJsonDraggable',        { fg = colors.purple,     bg = 'NONE' })  -- "draggable"
  highlight(0, 'scratchJsonRotationStyle',    { fg = colors.purple,     bg = 'NONE' })  -- "rotationStyle"

  -- Block properties
  highlight(0, 'scratchJsonOpcode',           { fg = colors.orange,     bg = 'NONE' })  -- "opcode"
  highlight(0, 'scratchJsonNext',             { fg = colors.blue,       bg = 'NONE' })  -- "next"
  highlight(0, 'scratchJsonParent',           { fg = colors.blue,       bg = 'NONE' })  -- "parent"
  highlight(0, 'scratchJsonInputs',           { fg = colors.blue,       bg = 'NONE' })  -- "inputs"
  highlight(0, 'scratchJsonFields',           { fg = colors.blue,       bg = 'NONE' })  -- "fields"
  highlight(0, 'scratchJsonShadow',           { fg = colors.blue,       bg = 'NONE' })  -- "shadow"
  highlight(0, 'scratchJsonTopLevel',         { fg = colors.blue,       bg = 'NONE' })  -- "topLevel"
  highlight(0, 'scratchJsonMutation',         { fg = colors.blue,       bg = 'NONE' })  -- "mutation"

  -- Opcodes by category
  highlight(0, 'scratchOpcodeMotion',         { fg = colors.blue,       bg = 'NONE' })  -- motion_*
  highlight(0, 'scratchOpcodeLooks',          { fg = colors.purple,     bg = 'NONE' })  -- looks_*
  highlight(0, 'scratchOpcodeSound',          { fg = colors.pink,       bg = 'NONE' })  -- sound_*
  highlight(0, 'scratchOpcodeEvent',          { fg = colors.orange,     bg = 'NONE' })  -- event_*
  highlight(0, 'scratchOpcodeControl',        { fg = colors.orange,     bg = 'NONE' })  -- control_*
  highlight(0, 'scratchOpcodeSensing',        { fg = colors.turquoise,  bg = 'NONE' })  -- sensing_*
  highlight(0, 'scratchOpcodeOperator',       { fg = colors.green,      bg = 'NONE' })  -- operator_*
  highlight(0, 'scratchOpcodeData',           { fg = colors.orange,     bg = 'NONE' })  -- data_*
  highlight(0, 'scratchOpcodeProcedures',     { fg = colors.pink,       bg = 'NONE' })  -- procedures_*
  highlight(0, 'scratchOpcodePen',            { fg = colors.turquoise,  bg = 'NONE' })  -- pen_*
  highlight(0, 'scratchOpcodeMusic',          { fg = colors.pink,       bg = 'NONE' })  -- music_*

  -- Asset properties
  highlight(0, 'scratchJsonAssetId',          { fg = colors.redLight,   bg = 'NONE' })  -- "assetId"
  highlight(0, 'scratchJsonMd5ext',           { fg = colors.redLight,   bg = 'NONE' })  -- "md5ext"
  highlight(0, 'scratchJsonDataFormat',       { fg = colors.turquoise,  bg = 'NONE' })  -- "dataFormat"


  -----------------------------------------------------------------------------
  -- Common Scratch Terms (for documentation/comments)

  -- Sprite-related
  highlight(0, 'scratchSprite',               { fg = colors.turquoise,  bg = 'NONE' })  -- Sprite
  highlight(0, 'scratchStage',                { fg = colors.turquoise,  bg = 'NONE' })  -- Stage
  highlight(0, 'scratchClone',                { fg = colors.turquoise,  bg = 'NONE' })  -- Clone

  -- Costume/Sound
  highlight(0, 'scratchCostume',              { fg = colors.purple,     bg = 'NONE' })  -- Costume
  highlight(0, 'scratchBackdrop',             { fg = colors.purple,     bg = 'NONE' })  -- Backdrop
  highlight(0, 'scratchSoundAsset',           { fg = colors.pink,       bg = 'NONE' })  -- Sound

  -- Block types
  highlight(0, 'scratchHatBlock',             { fg = colors.orange,     bg = 'NONE' })  -- Hat block
  highlight(0, 'scratchStackBlock',           { fg = colors.blue,       bg = 'NONE' })  -- Stack block
  highlight(0, 'scratchReporterBlock',        { fg = colors.green,      bg = 'NONE' })  -- Reporter block
  highlight(0, 'scratchBooleanBlock',         { fg = colors.turquoise,  bg = 'NONE' })  -- Boolean block
  highlight(0, 'scratchCBlock',               { fg = colors.orange,     bg = 'NONE' })  -- C block
  highlight(0, 'scratchCapBlock',             { fg = colors.orange,     bg = 'NONE' })  -- Cap block


  -----------------------------------------------------------------------------
  -- Treesitter Groups (for potential future parsers)

  highlight(0, '@keyword.scratch',            { fg = colors.blue,       bg = 'NONE' })  -- Keywords
  highlight(0, '@keyword.control.scratch',    { fg = colors.orange,     bg = 'NONE' })  -- Control keywords
  highlight(0, '@keyword.event.scratch',      { fg = colors.orange,     bg = 'NONE' })  -- Event keywords

  highlight(0, '@function.scratch',           { fg = colors.orange,     bg = 'NONE' })  -- Functions/blocks
  highlight(0, '@function.builtin.scratch',   { fg = colors.pink,       bg = 'NONE' })  -- Built-in blocks
  highlight(0, '@function.custom.scratch',    { fg = colors.pink,       bg = 'NONE' })  -- Custom blocks

  highlight(0, '@variable.scratch',           { fg = colors.orange,     bg = 'NONE' })  -- Variables
  highlight(0, '@variable.list.scratch',      { fg = colors.orange,     bg = 'NONE' })  -- Lists

  highlight(0, '@constant.scratch',           { fg = colors.purple,     bg = 'NONE' })  -- Constants

  highlight(0, '@number.scratch',             { fg = colors.greenLight, bg = 'NONE' })  -- Numbers

  highlight(0, '@string.scratch',             { fg = colors.redLight,   bg = 'NONE' })  -- Strings

  highlight(0, '@operator.scratch',           { fg = colors.green,      bg = 'NONE' })  -- Operators

  highlight(0, '@punctuation.bracket.scratch', { fg = colors.white,     bg = 'NONE' })  -- Brackets
  highlight(0, '@punctuation.delimiter.scratch', { fg = colors.white,   bg = 'NONE' })  -- Delimiters

  highlight(0, '@comment.scratch',            { fg = colors.red,        bg = 'NONE' })  -- Comments


  -----------------------------------------------------------------------------
  -- LSP Semantic Tokens

  highlight(0, '@lsp.type.variable.scratch',  { fg = colors.orange,     bg = 'NONE' })
  highlight(0, '@lsp.type.function.scratch',  { fg = colors.pink,       bg = 'NONE' })
  highlight(0, '@lsp.type.keyword.scratch',   { fg = colors.blue,       bg = 'NONE' })
  highlight(0, '@lsp.type.operator.scratch',  { fg = colors.green,      bg = 'NONE' })
  highlight(0, '@lsp.type.string.scratch',    { fg = colors.redLight,   bg = 'NONE' })
  highlight(0, '@lsp.type.number.scratch',    { fg = colors.greenLight, bg = 'NONE' })
  highlight(0, '@lsp.type.comment.scratch',   { fg = colors.red,        bg = 'NONE' })


  -----------------------------------------------------------------------------
  -- Snap! Blocks (Scratch derivative)

  highlight(0, 'snapblocksRing',              { fg = colors.pink,       bg = 'NONE' })  -- Ring (lambda)
  highlight(0, 'snapblocksUnevaluated',       { fg = colors.orange,     bg = 'NONE' })  -- Unevaluated inputs
  highlight(0, 'snapblocksMultiline',         { fg = colors.redLight,   bg = 'NONE' })  -- Multiline text
  highlight(0, 'snapblocksUpvar',             { fg = colors.orange,     bg = 'NONE' })  -- Script variables


  -----------------------------------------------------------------------------
  -- GP (Scratch derivative) Blocks

  highlight(0, 'gpBlocksStatement',           { fg = colors.blue,       bg = 'NONE' })  -- GP statements
  highlight(0, 'gpBlocksReporter',            { fg = colors.green,      bg = 'NONE' })  -- GP reporters
  highlight(0, 'gpBlocksBoolean',             { fg = colors.turquoise,  bg = 'NONE' })  -- GP booleans

end

return scratch
