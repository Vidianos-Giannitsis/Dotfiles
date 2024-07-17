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
	  ["Chemeng" , "https://www.chemeng.ntua.gr/", "ch"],
	  ["Wolfram Alpha" , "https://www.wolframalpha.com", "wa"],
	  ["Chemeng Forum" , "https://forum.chemeng.ntua.gr/", "cf"],
	  ["Science Direct" , "https://www.sciencedirect.com/", "sc"],
	  ["Research Rabbit" , "https://www.researchrabbitapp.com/home", "rr"],
	  ["SciHub", "https://sci-hub.se/", "sci"],
	  ["Scopus" , "https://www.scopus.com/search/form.uri?display=basic#basic", "sc"],
	  ["Helios", "https://helios.ntua.gr/my/", "h"]
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
	  ["Mobilizon", "https://mobilizon.fr/@m_x_research", "mb"],
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
	  ["RR Dex", "https://funnotbun.github.io/?table=speciesTable&", "rrp"],
	  ["RR Changelog", "https://docs.google.com/spreadsheets/d/1bhgRoclN54ur0NqjpqYzRO-OtNlFb1bWWhFilgzYOls/edit#gid=1231377635", "rrc"],
	  ["DuelingBook", "https://www.duelingbook.com/", "db"],
	  ["Master Duel Meta", "https://www.masterduelmeta.com/", "mdm"],
	  ["Pokerogue", "https://pokerogue.net/", "pr"],
	  ["Pack Opener", "https://db.ygoprodeck.com/pack-open/", "po"]
      ]
    ],
    [ "Draft League",
      [
	  ["JJDL Doc", "https://docs.google.com/spreadsheets/d/1_0v9tQIhlNfJ7FEJlrMIvMxnuvudB8TTBESqxDbA9WI/edit?gid=892898244#gid=892898244", "jj"],
	  ["DDL Doc", "https://docs.google.com/spreadsheets/d/1r0ehfi_wbVUk_eheVnw71HlbAqZA5etWl-8ZyYBRVLU/edit#gid=1680450484", "ddl"],
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
	  ["MIT SciML Course", "https://book.sciml.ai/", "mit"],
	  ["CRNN Examples", "https://github.com/DENG-MIT/CRNN?tab=readme-ov-file", "crnn"],
	  ["Raindrop", "https://app.raindrop.io/my/0", "rd"],
	  ["Hex Color Codes", "https://www.color-hex.com/", "ch"],
	  ["Word Counter", "https://wordcounter.net/", "wc"],
	  ["Q-Dance Network", "https://www.q-dance.com/network/library", "qd"],
	  ["Euraxess", "https://euraxess.ec.europa.eu/jobs/search?keywords=&f%5B0%5D=job_country%3A747&f%5B1%5D=job_country%3A751&f%5B2%5D=job_country%3A757&f%5B3%5D=job_country%3A760&f%5B4%5D=job_country%3A768&f%5B5%5D=job_country%3A770&f%5B6%5D=job_country%3A781&f%5B7%5D=job_country%3A791&f%5B8%5D=job_country%3A792&f%5B9%5D=job_country%3A794&f%5B10%5D=job_country%3A796&f%5B11%5D=job_country%3A798&f%5B12%5D=job_country%3A799&f%5B13%5D=job_research_field%3A39&f%5B14%5D=job_research_field%3A50&f%5B15%5D=job_research_field%3A61&f%5B16%5D=job_research_field%3A88&f%5B17%5D=job_research_field%3A133&f%5B18%5D=job_research_field%3A167&f%5B19%5D=job_research_field%3A169&f%5B20%5D=job_research_field%3A174&f%5B21%5D=job_research_field%3A178&f%5B22%5D=job_research_field%3A181&f%5B23%5D=job_research_field%3A186&f%5B24%5D=job_research_field%3A188&f%5B25%5D=job_research_field%3A195&f%5B26%5D=job_research_field%3A197&f%5B27%5D=job_research_field%3A199&f%5B28%5D=job_research_field%3A402&f%5B29%5D=job_research_field%3A409&f%5B30%5D=job_research_field%3A410&f%5B31%5D=job_research_field%3A428&f%5B32%5D=job_research_profile%3A447", "eu"],
	  ["Detexify", "https://detexify.kirelabs.org/classify.html", "dt"]
      ]
    ],
];
var feeds = [
];
