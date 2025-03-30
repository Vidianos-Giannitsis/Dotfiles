// All the variable for the app
// Their name are explicit

var username = "Vidianos"
var clientid = "be212a58528168962a39c64052c1d88e";
var redirecturi = "http://localhost:8002/callback.html";
var locations = [
  "784201",
  "784302"
];
var images = [
  "1412446496031.jpg",
  "1417804954510.jpg",
  "1424655132831.jpg",
  "1429482830109.jpg",
  "1437167260211.jpg",
  "1437214448937.jpg",
  "1444816416285.jpg",
  "1444827390885.jpg",
  "1444827771939.jpg",
  "1445223016379.jpg",
  "1445223050369.jpg",
  "1445705808951.jpg",
  "1445713184723.jpg",
  "1448399280112.jpg",
  "1450066383293.jpg",
  "1450074394745.jpg"
];
var searchs = [
    ["!g", "https://www.google.com/search?q="],
    ["!y", "https://www.youtube.com/results?search_query="],
    ["!a", "https://wiki.archlinux.org/index.php?search="],
    ["!r", "https://www.reddit.com/search/?q="],
    ["!l", "https://lutris.net/games?q="],
    ["!p", "https://www.protondb.com/search?q="],
    ["!c", "https://lolchess.gg/search?region=EUNE&name="],
    ["!w", "https://www.wolframalpha.com/input/?i="],
    ["!s", "https://www.sciencedirect.com/search?qs="],
    ["!t", "https://translate.google.com/?sl=auto&tl=en&text="],
    ["!b", "https://bulbapedia.bulbagarden.net/w/index.php?title="]
];
var favorites = [
    [ "University",
      [
	  ["My KULeuven" , "https://admin.kuleuven.be/mykuleuven/en/mykuleuven?project=TL&pagina=Home&thema=Banner&onderwerp=MyKULeuven", "mku"],
	  ["KU Leuven Sports" , "https://www.kuleuven.be/sport/eng/offer/sessions", "kus"],
	  ["Life KU Leuven" , "https://www.kuleuven.be/english/life-at-ku-leuven", "lku"],
	  ["KU Leuven Mail", "https://outlook.office.com/mail/", "kum"],
	  ["SciHub", "https://sci-hub.se/", "sci"],
	  ["Scopus" , "https://www.scopus.com/search/form.uri?display=basic#basic", "sc"],
	  ["Research Rabbit" , "https://www.researchrabbitapp.com/home", "rr"],
	  ["NTUA" , "https://www.chemeng.ntua.gr/", "ch"]
      ]
    ],
    [ "Socials",
      [
	  ["Messenger" , "https://www.messenger.com", "ms"],
	  ["Twitch" , "https://www.twitch.tv", "tw"],
	  ["Github" , "https://github.com/Vidianos-Giannitsis", "gh"],
	  ["Youtube" , "https://www.youtube.com/", "yt"],
	  ["Discord" , "https://discord.com/channels/@me", "dc"],
	  ["Reddit", "https://www.reddit.com/", "rd"],
	  ["Matrix", "https://app.element.io/", "mtr"],
	  ["Slack", "https://app.slack.com/client/T035G68PP", "sl"],
	  ["Instagram" , "https://instagram.com/", "in"]
      ]
    ],
    [ "Documentation pages",
      [
	  ["Emacs", "https://www.gnu.org/software/emacs/manual/html_node/emacs/index.html", "em"],
	  ["Octave" , "https://octave.org/doc/v5.2.0/", "oc"],
	  ["Org-Roam" , "https://www.orgroam.com/manual.html", "or"],
	  ["Org-Roam-Bibtex" , "https://github.com/org-roam/org-roam-bibtex", "orb"],
	  ["Org-Roam Discourse", "https://org-roam.discourse.group/", "ord"],
	  ["Qtile", "http://docs.qtile.org/en/latest/", "qt"],
	  ["Clojure", "https://www.braveclojure.com/clojure-for-the-brave-and-true/", "clj"]
      ]
    ],
    [ "Games",
      [
	  ["Dod", "https://www.dod.gr", "d"],
	  ["LolChess Account", "https://lolchess.gg/profile/eune/auroradraco", "lcm"],
	  ["Proton", "https://www.protondb.com/", "pr"],
	  ["MC Biome Finder", "https://www.chunkbase.com/apps/biome-finder#-3038289977291799158", "bf"],
	  ["Smogon", "https://smogon.com", "sm"],
	  ["Showdown", "https://play.pokemonshowdown.com/", "ps"],
	  ["Damage Calc", "https://calc.pokemonshowdown.com/index.html", "calc"],
	  ["Type Chart", "https://www.smogon.com/dex/sm/types/", "tc"],
	  ["ER Dex", "https://forwardfeed.github.io/ER-nextdex/static/", "erp"],
	  ["RR Changelog", "https://docs.google.com/spreadsheets/d/1bhgRoclN54ur0NqjpqYzRO-OtNlFb1bWWhFilgzYOls/edit#gid=1231377635", "rrc"],
	  ["DuelingBook", "https://www.duelingbook.com/", "db"],
	  ["Master Duel Meta", "https://www.masterduelmeta.com/", "mdm"],
	  ["Pokerogue", "https://pokerogue.net/", "pr"],
	  ["Pack Opener", "https://db.ygoprodeck.com/pack-open/", "po"]
      ]
    ],
    [ "Draft League",
      [
	  ["JJDL Doc", "https://docs.google.com/spreadsheets/d/1QbjD_jqq_643EwEWxh4dDLaOwXKWZ6-ypXdmrRLuxAU/edit?gid=1008590874#gid=1008590874", "jj"],
	  ["MGDL Doc", "https://docs.google.com/spreadsheets/d/1VKD45WdA4XbukBrKSq3PD3ZUrK-ag9LFvzmQT_pOFck/edit?gid=0#gid=0", "mgdl"],
	  ["89Goals Doc", "https://docs.google.com/spreadsheets/d/1Kd_pb-HK7cLG5_dqlxZZ03SCin0D_8lA2Dj7npWQpJw/edit?gid=426331160#gid=426331160", "89g"],
	  ["Techno Prep Doc", "https://docs.google.com/spreadsheets/d/14RJij-bQA8fHorJzyh2w9HedpbgUhTYP2UiqasM9Ba0/edit#gid=359842964", "tpd"],
	  ["TeamHawk Prep Doc", "https://docs.google.com/spreadsheets/d/1wM7kwLVnemo4TKX2nPLJmSaCRgDqiSyffcdszLPrYPM/edit#gid=439174144", "hpd"],
	  ["RR Showdown", "https://play.radicalred.net/", "rrs"],
	  ["DL Showdown", "https://dl.psim.us/", "dl"],
	  ["Damage Calc", "https://calc.pokemonshowdown.com/index.html", "calc"],
	  ["RR Calc", "https://calc.radicalred.net/", "rcalc"]
      ]
    ],

    [ "Other",
      [
	  ["Darebee", "https://darebee.com/", "d"],
	  ["MIT Comp Thinking", "https://computationalthinking.mit.edu/Fall24/", "mit"],
	  ["CRNN Examples", "https://github.com/DENG-MIT/CRNN?tab=readme-ov-file", "crnn"],
	  ["Raindrop", "https://app.raindrop.io/my/0", "rd"],
	  ["Hex Color Codes", "https://www.color-hex.com/", "ch"],
	  ["Word Counter", "https://wordcounter.net/", "wc"],
	  ["ChatGPT", "https://chatgpt.com/", "gpt"],
	  ["FFXIII-LR Sequel", "https://archive.org/details/manga_Final_Fantasy_XIII_Reminiscence_Tracer_of_Memories/Final%20Fantasy%20XIII%20-%20Reminiscence%20-%20Tracer%20of%20Memories%20-%20Chapter%2000%20-%20Prologue/", "lr"],
	  ["FFXIII - Side Characters", "https://docs.google.com/document/d/1TqAq_CpKTp3anFU3fmwoVLFWVYyY0_yYrmv7nVnZ0lU/edit?tab=t.0", "ffsc"],
	  ["Detexify", "https://detexify.kirelabs.org/classify.html", "dt"]
      ]
    ],
];
var feeds = [
];
