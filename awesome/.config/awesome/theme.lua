---------------------------
-- Default awesome theme --
---------------------------
local theme_assets = require("beautiful.theme_assets")
local xresources = require("beautiful.xresources")
local naughty = require("naughty")
local dpi = xresources.apply_dpi

local gfs = require("gears.filesystem")
local themes_path = gfs.get_themes_dir()

colors_path = "/home/zovt/.colors"
function read_colors()
	local colors_file = io.open(colors_path, "r")
	if not colors_file then
		error("Could not open .colors")
	end
	local colors = {}
	while true do
		local line = colors_file:read("*line")
		if not line then
			break
		end
		local words = {}
		for word in line:gmatch("%S+") do
			words[#words+1] = word
		end
		colors[words[1]] = words[2]
	end
	return colors
end
colors = read_colors()

local theme = {}

theme.font          = "Iosevka Term Slab 14"

theme.bg_normal     = colors["background"]
theme.bg_focus      = colors["foreground"]
theme.bg_urgent     = "#ff0000"
theme.bg_minimize   = "#444444"
theme.bg_systray    = "#FFFFFF00"

theme.fg_normal     = colors["foreground"]
theme.fg_focus      = colors["background"]
theme.fg_urgent     = colors["foreground"]
theme.fg_minimize   = colors["foreground"]

theme.useless_gap   = dpi(0)
theme.border_width  = dpi(2)
theme.border_normal = colors["foreground"]
theme.border_focus  = colors["alt-background"]
theme.border_marked = "#91231c"

-- There are other variable sets
-- overriding the default one when
-- defined, the sets are:
-- taglist_[bg|fg]_[focus|urgent|occupied|empty|volatile]
-- tasklist_[bg|fg]_[focus|urgent]
-- titlebar_[bg|fg]_[normal|focus]
-- tooltip_[font|opacity|fg_color|bg_color|border_width|border_color]
-- mouse_finder_[color|timeout|animate_timeout|radius|factor]
-- prompt_[fg|bg|fg_cursor|bg_cursor|font]
-- hotkeys_[bg|fg|border_width|border_color|shape|opacity|modifiers_fg|label_bg|label_fg|group_margin|font|description_font]
-- Example:
--theme.taglist_bg_focus = "#ff0000"
theme.tasklist_bg_normal = "#FFFFFF00"

-- Generate taglist squares:
local taglist_square_size = dpi(4)
theme.taglist_squares_sel = theme_assets.taglist_squares_sel(
    taglist_square_size, theme.fg_normal
)
theme.taglist_squares_unsel = theme_assets.taglist_squares_unsel(
    taglist_square_size, theme.fg_normal
)

-- Variables set for theming notifications:
-- notification_font
-- notification_[bg|fg]
-- notification_[width|height|margin]
-- notification_[border_color|border_width|shape|opacity]

-- Variables set for theming the menu:
-- menu_[bg|fg]_[normal|focus]
-- menu_[border_color|border_width]
theme.menu_submenu_icon = themes_path.."default/submenu.png"
theme.menu_height = dpi(15)
theme.menu_width  = dpi(100)

-- You can add as many variables as
-- you wish and access them by using
-- beautiful.variable in your rc.lua
--theme.bg_widget = "#cc0000"

-- Define the image to load
theme.wallpaper = "/home/zovt/.config/awesome/king_shift.jpg"
theme.wallpapers = {
	"/home/zovt/.config/awesome/solaire_shift.png",
	"/home/zovt/.config/awesome/king_shift.jpg",
}

-- Generate Awesome icon:
theme.awesome_icon = theme_assets.awesome_icon(
    theme.menu_height, theme.bg_focus, theme.fg_focus
)

-- Define the icon theme for application icons. If not set then the icons
-- from /usr/share/icons and /usr/share/icons/hicolor will be used.
theme.icon_theme = "Flat-Remix-Blue"

return theme

-- vim: filetype=lua:expandtab:shiftwidth=4:tabstop=8:softtabstop=4:textwidth=80
