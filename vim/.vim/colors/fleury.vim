hi clear

if exists("syntax_on")
  syntax reset
endif

let g:colors_name = "fleury"

" Palette
let s:rich_black         = "#020202"
let s:light_bronze       = "#b99468"
let s:charcoal_gray_lite = "#1e1e1e"
let s:dark_slate         = "#222425"
let s:amber_gold         = "#fcaa05"
let s:medium_gray        = "#404040"
let s:dim_gray           = "#666666"
let s:goldenrod          = "#f0c674"
let s:bright_orange      = "#ffaa00"
let s:dusty_rose         = "#dc7575"
let s:sunflower_yellow   = "#edb211"
let s:burnt_orange       = "#de451f"
let s:sky_blue           = "#2895c7"
let s:sky_blue_lite      = "#2f2f38"
let s:bright_red         = "#ff0000"
let s:fresh_green        = "#66bc11"
let s:lime_green         = "#003939"
let s:vivid_vermilion    = "#f0500c"
let s:golden_yellow      = "#f0bb0c"
let s:pure_black         = "#000000"

let s:mode_fg            = "#e7aa4d"
let s:mode_bg            = "#1a120b"

" UI
execute 'hi Normal guifg='       . s:light_bronze . ' guibg=' . s:rich_black
execute 'hi Cursor guifg='       . s:rich_black   . ' guibg=' . s:fresh_green
execute 'hi Visual guibg='       . s:lime_green
execute 'hi LineNr guifg='       . s:medium_gray  . ' guibg=' . s:rich_black
execute 'hi CursorLine guibg='   . s:charcoal_gray_lite
execute 'hi CursorLineNr guifg=' . s:light_bronze . ' guibg=' . s:charcoal_gray_lite
execute 'hi StatusLine guifg='   . s:mode_fg      . ' guibg=' . s:mode_bg . ' gui=bold'
execute 'hi StatusLineNC guifg=' . s:mode_fg      . ' guibg=' . s:rich_black

" Syntax
execute 'hi Comment guifg='      . s:dim_gray     . ' gui=italic'
execute 'hi Keyword guifg='      . s:goldenrod
execute 'hi Statement guifg='    . s:goldenrod
execute 'hi String guifg='       . s:bright_orange
execute 'hi Constant guifg='     . s:bright_orange
execute 'hi Function guifg='     . s:burnt_orange
execute 'hi Identifier guifg='   . s:light_bronze
execute 'hi Type guifg='         . s:sunflower_yellow
execute 'hi PreProc guifg='      . s:dusty_rose
execute 'hi Special guifg='      . s:sky_blue

" Search
execute 'hi Search guifg='       . s:pure_black . ' guibg=' . s:golden_yellow
execute 'hi IncSearch guifg='    . s:pure_black . ' guibg=' . s:vivid_vermilion

" Errors
execute 'hi Error guifg='        . s:bright_red
execute 'hi WarningMsg guifg='   . s:bright_red


execute 'hi NonText guifg=' . s:burnt_orange
execute 'hi Directory guifg=' . s:burnt_orange
execute 'hi TabLine guifg='     . s:dim_gray      . ' guibg=' . s:charcoal_gray_lite
execute 'hi TabLineFill guibg=' . s:rich_black

execute 'hi Pmenu guibg=' . s:rich_black
execute 'hi PmenuSel guibg=' . s:burnt_orange


