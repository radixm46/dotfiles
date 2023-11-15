-- Pull in the wezterm API
local wezterm = require 'wezterm';
local config  = wezterm.config_builder()
local act     = wezterm.action

-------------------------------- configure fonts --------------------------------------
do
   config.font = wezterm.font_with_fallback {
      -- user defined fonts
      {
         family = "UDEV Gothic NFLG",
         harfbuzz_features = {
            "calt=1",
            "clig=1",
            "liga=1",
         },
      },
      "UDEV Gothic NF",
      "HackGenNerd Console",
      -- BuiltIn fonts
      "JetBrains Mono",
      -- Assumed to have Emoji Presentation
      -- Pixel sizes: [128]
      "Noto Color Emoji",
      "Symbols Nerd Font Mono",
      -- fallback
      "Last Resort High-Efficiency",
   }
   config.font_size = os.getenv("Darwin")
      and 15.0
      or 14.5
end

-------------------------------- window appearance ------------------------------------
do
   local color_scheme        = "ayu"
   config.color_scheme       = color_scheme
   local color_palette       = wezterm.color.get_builtin_schemes()[color_scheme]
   config.window_decorations = "RESIZE"

   function isBgDark(c)
      _, _, l, _ = wezterm.color.parse(c):hsla()
      return l < 0.55
   end

   config.window_background_opacity = os.getenv("Darwin")
      and 0.85
      or  isBgDark(color_palette.background)
          and 0.60
          or  0.85 -- if bg seems 'light', less transparent bg

   config.macos_window_background_blur = 35
   config.window_padding = {
      left   = 2,
      right  = 2,
      top    = 0,
      bottom = 0,
   }

   ---- define tab style ----
   -- use customized classic style tab bar
   config.use_fancy_tab_bar            = false
   config.tab_bar_at_bottom            = false
   config.hide_tab_bar_if_only_one_tab = true
   config.tab_max_width                = 50 -- for longer tab name

   -- active
   local active_bg_color   = color_palette.cursor_bg
   local active_fg_color   = color_palette.background
   -- inactive
   local inactive_bg_color = color_palette.background
   local inactive_fg_color = color_palette.foreground
   -- hover
   local hover_bg_color    = color_palette.selection_bg or "orange"
   local hover_fg_color    = color_palette.selection_fg or "black"

   function transparentColor(c, a)
      local r, g, b, _ = wezterm.color.parse(c):srgba_u8()
      return string.format("rgba(%d %d %d %f)", r, g, b, a)
   end

   config.colors = {
      tab_bar = {
         -- transparent tab bar bg
         background = transparentColor(inactive_bg_color,
                                       config.window_background_opacity + 0.1),
         -- The active tab is the one that has focus in the window
         active_tab = {
            bg_color = active_bg_color,
            fg_color = active_fg_color,
            -- Specify whether you want "Half", "Normal" or "Bold" intensity for the
            -- label shown for this tab.
            -- The default is "Normal"
            intensity = 'Bold',
            -- Specify whether you want "None", "Single" or "Double" underline for
            -- label shown for this tab.
            -- The default is "None"
            underline = 'None',
            -- Specify whether you want the text to be italic (true) or not (false)
            -- for this tab.  The default is false.
            italic = false,
            -- Specify whether you want the text to be rendered with strikethrough (true)
            -- or not for this tab.  The default is false.
            strikethrough = false,
         },
         -- Inactive tabs are the tabs that do not have focus
         inactive_tab = {
            bg_color  = inactive_bg_color,
            fg_color  = inactive_fg_color,
            intensity = 'Half',
         },
         inactive_tab_hover = {
            bg_color  = hover_bg_color,
            fg_color  = hover_fg_color,
            intensity = 'Normal',
         },
         -- The new tab button that let you create new tabs
         new_tab = {
            bg_color  = inactive_bg_color,
            fg_color  = inactive_fg_color,
            intensity = 'Normal',
         },
         new_tab_hover = {
            bg_color  = hover_bg_color,
            fg_color  = hover_fg_color,
            intensity = 'Bold',
         },
      },
      visual_bell = color_palette.background,
   }

   ---- dim inactive panes ----
   config.inactive_pane_hsb = {
      saturation = 0.9,
      brightness = 0.8,
   }


   ---- configure tab bar ----
   -- This function returns the suggested title for a tab.
   -- It prefers the title that was set via `tab:set_title()`
   -- or `wezterm cli set-tab-title`, but falls back to the
   -- title of the active pane in that tab.
   function tab_title(tab_info)
      local title = tab_info.tab_title
      -- if the tab title is explicitly set, take that
      if title and #title > 0 then
         return title
      end
      -- Otherwise, use the title from the active pane
      -- in that tab
      return tab_info.active_pane.title
   end

   local TAB_LEFT_SEPARATOR  = wezterm.nerdfonts.ple_pixelated_squares_big_mirrored
   local TAB_RIGHT_SEPARATOR = wezterm.nerdfonts.ple_pixelated_squares_big
   wezterm.on(
      'format-tab-title',
      function(tab, tabs, panes, config, hover, max_width)
         local edge_background = inactive_bg_color
         local background = inactive_bg_color
         local foreground = inactive_fg_color
         if tab.is_active then
            background = active_bg_color
            foreground = active_fg_color
         elseif hover then
            background = hover_bg_color
            foreground = hover_fg_color
         end

         local edge_foreground = background

         local title = tab_title(tab)

         -- ensure that the titles fit in the available space,
         -- and that we have room for the edges.
         title = wezterm.truncate_right(title, max_width - 2)

         return {
            { Background = { Color = edge_background } },
            { Foreground = { Color = edge_foreground } },
            { Text = TAB_LEFT_SEPARATOR },
            { Background = { Color = edge_foreground } },
            { Foreground = { Color = edge_background } },
            { Text = TAB_RIGHT_SEPARATOR },
            { Background = { Color = background } },
            { Foreground = { Color = foreground } },
            { Text = ' ' .. title }, -- paddling
            { Background = { Color = edge_foreground } },
            { Foreground = { Color = edge_background } },
            { Text = TAB_LEFT_SEPARATOR },
            { Background = { Color = edge_background } },
            { Foreground = { Color = edge_foreground } },
            { Text = TAB_RIGHT_SEPARATOR },
         }
      end
   )

   ---- visual bell config ----
   config.front_end     = "WebGpu" -- default
   config.audible_bell  = "Disabled"
   config.animation_fps = 60
   config.visual_bell   = {
      fade_in_function     = 'EaseIn',
      fade_in_duration_ms  = 30,
      fade_out_function    = 'EaseOut',
      fade_out_duration_ms = 220,
   }
end

-------------------------------- configure inputs -------------------------------------
do
   -- Rather than emitting fancy composed characters when alt is pressed, treat the
   -- input more like old school ascii with ALT held down
   -- if on macOS, use left alt as composed key
   config.send_composed_key_when_right_alt_is_pressed = false
   config.send_composed_key_when_left_alt_is_pressed  = os.getenv("Darwin")
      and true
      or false
   -- enable ime with macOS
   config.use_ime = os.getenv("Darwin")
      and false
      or true
   config.macos_forward_to_ime_modifier_mask = "SHIFT | CTRL"
end

-- provide config
return config
