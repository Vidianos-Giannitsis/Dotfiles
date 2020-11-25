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
terminal = guess_terminal()
editor = "emacs"
min_browser = "qutebrowser"
full_browser = "brave"
office = "libreoffice"
file_manager = "thunar" # I mostly use dired inside emacs for file management, but this exists as a gui file manager in case I need it
music_player = "spotify"
calculator = "qalculate-gtk"
pdf_reader = "zathura"
screenshots = "flameshot gui"

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
Key2("M-C-x", lazy.window.kill(), desc="Kill focused window"),
Key2("M-S-r", lazy.restart(), desc="Restart qtile"),
Key2("M-S-e", lazy.shutdown(), desc="Shutdown qtile"),

Key2("M-<Return>", lazy.spawn(terminal), desc="Launch terminal"),
Key2("M-r", lazy.spawncmd(),
    desc="Spawn a command using a prompt widget"),
Key2("M-S-b", lazy.spawn(min_browser), desc="Launch minimal browser"),
Key2("M-C-b", lazy.spawn(full_browser), desc="Launch full browser"),
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
Key2("M-C-z", lazy.spawn(pdf_reader), desc="Launch pdf reader"),

Key2("<XF86AudioRaiseVolume>", lazy.spawn("amixer set Master 5%+ unmute"), desc="Raise Volume and unmute if muted"),
    Key2("<XF86AudioLowerVolume>", lazy.spawn("amixer set Master 5%- unmute"), desc="Lower Volume and unmute if muted"),
    Key2("<XF86AudioMute>", lazy.spawn("amixer set Master toggle"), desc="Mute audio"),
    Key2("<XF86MonBrightnessUp>", lazy.spawn("xbacklight -inc 10"), desc="Raise Brightness"),
    Key2("<XF86MonBrightnessDown>", lazy.spawn("xbacklight -dec 10"), desc="Lower Brightness"),
    Key2("<Print>", lazy.spawn(screenshots), desc="Screenshot util"),
]

groups = [
    ScratchPad("scratchpad", [
	DropDown("music", "spotify", opacity=0.8),
	DropDown("term", "alacritty", opacity=0.8),
	DropDown("calc", "qalculate-gtk", opacity=0.8),
	DropDown("obs", "obs", opacity=0.8) ]),
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
	     Key2("M-S-o", lazy.group['scratchpad'].dropdown_toggle('obs')),
	     ])

# Layouts
layouts = [
    layout.MonadTall(border_focus = "#005858", border_normal = "#002525"),
    # layout.Columns(),
    # layout.Bsp(),
    layout.Max(),
    layout.Stack(num_stacks=2, border_focus = "#005858", border_normal = "#002525"),
    # layout.Matrix(),
    layout.MonadWide(border_focus = "#005858", border_normal = "#002525"),
    # layout.RatioTile(),
    # layout.Tile(border_focus = "#005858", border_normal = "#002525", border_width ="2"),
    # layout.TreeTab(),
    # layout.VerticalTile(),
    # layout.Zoomy(),
]

widget_defaults = dict(
    font='sans',
    fontsize=11,
    padding=2,
    background="#002b36",
    foreground="#fdf6e3",
)
extension_defaults = widget_defaults.copy()

screens = [
    Screen(
        bottom=bar.Bar(
            [
                widget.CurrentLayout(background="074231"),
                widget.Sep(foreground="#421307", size_percent=100),
                widget.GroupBox(),
                widget.Prompt(),
                widget.WindowName(),
                widget.Sep(foreground="#421307", size_percent=100),
                widget.TextBox("RAM: ", background="#074231"),
                widget.Memory(background="#074231"),
                widget.TextBox(",", background="#074231"),
                widget.CPU(background="#074231"),
                widget.TextBox("Volume: ", background="071942"),
                widget.PulseVolume(background="071942"),
                widget.TextBox("Battery: ", background="#074231"),
                widget.Battery(low_percentage = 0.2,
                               notify_below = 0.15,
                               update_interval = 30,
                               discharge_char = '↓',
                               charge_char = '↑',
                               background="#074231",
                               ),
                widget.TextBox("", background="071942"),
                widget.Clock(format='%a %d-%m-%Y %I:%M %p', background="071942"),
                widget.Sep(foreground="#421307", size_percent=100),
                widget.Systray(),
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
    {'wmclass': 'confirm'},
    {'wmclass': 'dialog'},
    {'wmclass': 'download'},
    {'wmclass': 'error'},
    {'wmclass': 'file_progress'},
    {'wmclass': 'notification'},
    {'wmclass': 'splash'},
    {'wmclass': 'toolbar'},
    {'wmclass': 'confirmreset'},  # gitk
    {'wmclass': 'makebranch'},  # gitk
    {'wmclass': 'maketag'},  # gitk
    {'wname': 'branchdialog'},  # gitk
    {'wname': 'pinentry'},  # GPG key password entry
    {'wmclass': 'ssh-askpass'},  # ssh-askpass
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
