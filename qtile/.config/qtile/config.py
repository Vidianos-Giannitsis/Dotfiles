from libqtile.command import lazy
from typing import List  # noqa: F401
from libqtile import bar, layout, widget, hook
from libqtile.config import Click, Drag, Group, Screen, Key, Match, Rule, ScratchPad, DropDown
from libqtile.config import EzKey as Key2
from libqtile.lazy import lazy
from libqtile.utils import guess_terminal
import os
import subprocess

mod = "mod4"
terminal = "alacritty"
editor = "emacsclient -c -a emacs ~"
browser_2 = "firefox"
browser_1 = "brave"
office = "libreoffice"
file_manager = "thunar" # I mostly use dired inside emacs for file management, but this exists as a gui file manager in case I need it
music_player = "spotify"
calculator = "qalculate-gtk"
screenshots = "flameshot gui"
run_launcher = 'dmenu_run -fn "Source Code Pro Bold" -i -nb "#292d3e" -sb "#5b76b2" -nf "#839496" -sf "#eeffff" -p "Launch program"'

@hook.subscribe.startup_once
def autostart():
    home = os.path.expanduser('~/.config/qtile/autostart.sh')
    subprocess.call([home])

@hook.subscribe.client_new
def floating_calc(c):
    if c.name == "Qalculate!":
        c.toggle_floating()

keys = [
    Key2("M-k", lazy.layout.down(),
	desc="Move focus down in stack pane"),
    Key2("M-j", lazy.layout.up(),
	desc="Move focus up in stack pane"),
    Key2("M-h", lazy.layout.left(),
	 desc="Move focus left in stack pane"),
    Key2("M-l", lazy.layout.left(),
	 desc="Move focus right in stack pane"),

    Key2("M-A-k", lazy.layout.shuffle_down(),
	desc="Move window down in current stack "),
    Key2("M-A-j", lazy.layout.shuffle_up(),
	desc="Move window up in current stack "),
    Key2("M-A-h", lazy.layout.shuffle_left(),
	desc="Move window left in current stack "),
    Key2("M-A-l", lazy.layout.shuffle_right(),
	desc="Move window right in current stack "),

    Key2("M-<space>", lazy.layout.next(),
	desc="Switch window focus to other pane(s) of stack"),
    Key2("M-S-<space>", lazy.layout.rotate(),
	desc="Swap panes of split stack"),

    Key2("M-i", lazy.layout.grow()),
    Key2("M-d", lazy.layout.shrink()),
    Key2("M-n", lazy.layout.normalize()),
    Key2("M-m", lazy.layout.maximize()),
    Key2("M-f", lazy.layout.flip()),

    Key2("M-<Tab>", lazy.next_layout(), desc="Toggle between layouts"),

Key2("M-S-f", lazy.window.toggle_floating, desc="Toggle Floating"),
Key2("M-q", lazy.window.kill(), desc="Kill focused window"),
Key2("M-S-r", lazy.restart(), desc="Restart qtile"),
Key2("M-S-e", lazy.spawn("arcolinux-logout"), desc="Log out of qtile"),

Key2("M-<Return>", lazy.spawn(terminal), desc="Launch terminal"),
Key2("M-r", lazy.spawn(run_launcher),
    desc="Spawn a command using a prompt widget"),
Key2("M-S-b", lazy.spawn(browser_2), desc="Launch secondary browser"),
Key2("M-C-b", lazy.spawn(browser_1), desc="Launch main browser"),
Key2("M-C-f", lazy.spawn(file_manager), desc="Launch file manager"),
Key2("M-C-e", lazy.spawn(editor), desc="Launch Emacs"),
Key2("M-C-l", lazy.spawn(office), desc="Launch an office suite"),
Key2("M-S-l", lazy.spawn("lutris"), desc="Launch lutris"),
Key2("M-S-s", lazy.spawn("steam"), desc="Launch steam"),
Key2("M-C-d", lazy.spawn("discord"), desc="Launch discord"),
Key2("M-C-o", lazy.spawn("octave --gui"), desc="Launch octave"),
Key2("M-C-t", lazy.spawn("thunderbird"), desc="Launch email client"),
Key2("M-C-a", lazy.spawn("pavucontrol"), desc="Launch audio control"),
Key2("M-C-g", lazy.spawn("geogebra"), desc="Launch geogebra"),
Key2("M-C-i", lazy.spawn("inkscape"), desc="Launch inkscape"),
Key2("M-S-c", lazy.spawn("tuxedo-control-center"), desc="Launch computer settings manager"),
Key2("M-C-p", lazy.spawn("system-config-printer"), desc="Launch printer software"),
Key2("M-t", lazy.spawn("rofi-theme-selector"), desc="Launch a theme selector using rofi"),

Key2("<XF86AudioRaiseVolume>", lazy.spawn("amixer -c 1 sset Master 5%+ unmute"), desc="Raise Volume and unmute if muted"),
Key2("<XF86AudioLowerVolume>", lazy.spawn("amixer -c 1 sset Master 5%- unmute"), desc="Lower Volume and unmute if muted"),
Key2("<XF86AudioMute>", lazy.spawn("amixer -c sset Master toggle"), desc="Mute audio"),
Key2("<XF86MonBrightnessUp>", lazy.spawn("sudo brightnessctl -q s +10%"), desc="Raise Brightness"),
Key2("<XF86MonBrightnessDown>", lazy.spawn("sudo brightnessctl -q s 10%-"), desc="Lower Brightness"),
Key2("<Print>", lazy.spawn(screenshots), desc="Screenshot util"),
]

groups = [
    ScratchPad("scratchpad", [
    DropDown("music", "spotify", opacity=0.8, height=0.8, weight=0.8),
    DropDown("term", "alacritty", opacity=0.8),
    DropDown("calc", "qalculate-gtk", opacity=0.8),
    DropDown("emacs", "emacs scratchpad.org", width=0.4, height=0.5, opacity=0.8)
    ]),
    Group("1"),
    Group("2"),
    Group("3"),
    Group("4"),
    Group("5"),
    Group("6"),
    Group("7"),
    Group("8"),
    Group("9"),
]

for i in "123456789":
    keys.extend([
	Key([mod], i, lazy.group[i].toscreen(),
	    desc="Switch to group {}".format(i)),

	Key([mod, "shift"], i, lazy.window.togroup(i, switch_group=False),
	    desc="Switch to & move focused window to group {}".format(i)),
])

keys.extend([Key2("M-C-s", lazy.group['scratchpad'].dropdown_toggle('music')),
	     Key2("M-S-<Return>", lazy.group['scratchpad'].dropdown_toggle('term')),
	     Key2("M-C-c", lazy.group['scratchpad'].dropdown_toggle('calc')),
	     Key2("M-e", lazy.group['scratchpad'].dropdown_toggle('emacs')),
	     ])

# Layouts

def init_layout_theme():
    return {"margin":0,
	    "border_width":2,
	    "border_focus": "#5b76b2",
	    "border_normal": "#002525"
	    }

layout_theme = init_layout_theme()

layouts = [
    layout.MonadTall(**layout_theme),
    # layout.Columns(),
    # layout.Bsp(),
    layout.Max(),
    layout.Stack(num_stacks=2, border_focus = "#41557f", border_normal = "#002525"),
    # layout.Matrix(),
    layout.MonadWide(**layout_theme),
    layout.RatioTile(**layout_theme),
    # layout.Tile(border_focus = "#005858", border_normal = "#002525", border_width ="2"),
    # layout.TreeTab(),
    # layout.VerticalTile(),
    # layout.Zoomy(),
]

widget_defaults = dict(
    font='Source Code Pro',
    fontsize=16,
    padding=2,
    background="#292d3e",
    foreground="#eeffff",
)
extension_defaults = widget_defaults.copy()

screens = [
    Screen(
	bottom=bar.Bar(
	    [
		widget.CurrentLayout(background="#5b76b2"),
		widget.Sep(foreground="#363428", size_percent=100),
		widget.GroupBox(),
		widget.Prompt(),
		widget.WindowName(),
		widget.Sep(foreground="#363428", size_percent=100),
		widget.TextBox("Free Space: ", background="#5b76b2"),
		widget.DF(background="#5b76b2", visible_on_warn=False, format='({uf}{m}|{r:.0f}%)', warn_space=20),
		widget.Sep(foreground="#363428", size_percent=100),
		widget.TextBox("RAM: ", background="#242837"),
		widget.Memory(background="#242837"),
		widget.TextBox(",", background="#242837"),
		widget.CPU(background="#242837"),
		widget.TextBox("Battery: ", background="#5b76b2"),
		widget.Battery(low_percentage = 0.2,
			       notify_below = 0.15,
			       update_interval = 30,
			       discharge_char = '↓',
			       charge_char = '↑',
			       background="#5b76b2",
			       ),
		widget.TextBox("", background="#242837"),
		widget.Clock(format='%a %d-%m-%Y %I:%M %p', background="#242837"),
		widget.Sep(foreground="#363428", size_percent=100),
		widget.Systray(background="5b76b2"),
		widget.Sep(foreground="#363428", size_percent=100),
		widget.Pomodoro(length_long_break=20, color_inactive="#808080", color_active="6b83b9", color_break="516aa0"),
	],
	    24,
	),
    ),
]

mouse = [
    Drag([mod], "Button1", lazy.window.set_position_floating(),
	 start=lazy.window.get_position()),
    Drag([mod], "Button3", lazy.window.set_size_floating(),
	 start=lazy.window.get_size()),
    Click([mod], "Button2", lazy.window.bring_to_front())
]

floating_layout = layout.Floating(float_rules=[
    # Run the utility of `xprop` to see the wm class and name of an X client.
    *layout.Floating.default_float_rules,
    Match(title='Qalculate!'),  # qalculate-gtk
])

dgroups_key_binder = None
dgroups_app_rules = []  # type: List
main = None  # WARNING: this is deprecated and will be removed soon
follow_mouse_focus = True
bring_front_click = False
cursor_warp = False
auto_fullscreen = True
focus_on_window_activation = "smart"

# XXX: Gasp! We're lying here. In fact, nobody really uses or cares about this
# string besides java UI toolkits; you can see several discussions on the
# mailing lists, GitHub issues, and other WM documentation that suggest setting
# this string if your java app doesn't work correctly. We may as well just lie
# and say that we're a working one by default.
#
# We choose LG3D to maximize irony: it is a 3D non-reparenting WM written in
# java that happens to be on java's whitelist.
wmname = "LG3D"
