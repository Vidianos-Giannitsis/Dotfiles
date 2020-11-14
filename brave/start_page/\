const NAME = "Vidis";
const WELCOME_MESSAGE_TEMPLATE = ["night", "morning", "afternoon", "evening"];

// All shortcuts are in a `SHORTCUT_STARTER+shortcutKey` format. 
// So, for example, pressing `tab+q` would redirect you to https://google.com/?q=q
const SHORTCUT_STARTER = 'tab' 

// How much time (in milliseconds) you have to press shortcutKey after pressing SHORTCUT_STARTER.
// Also change --SHORTCUT_TIMEOUT in styles.css if you change this option.
const SHORTCUT_TIMEOUT = 1500;

// The groups of links are generated from this object. Edit it to edit the page's contents.
// shortcutKey must hold an all-lowercase single button. Theoretically should work with values like `esc` and `f1`,
// but intended to be used with just regular latin letters.
const MASTER_MAP = [
    {
        "groupName": "University",
        "items":[
            {"name": "Chemeng", "shortcutKey": "c", "url": "https://www.chemeng.ntua.gr/"},
            {"name": "MyCourses", "shortcutKey": "m", "url": "https://mycourses.ntua.gr/index.php"},
            {"name": "ChemengCourses", "shortcutKey": "C", "url": "https://courses.chemeng.ntua.gr/"}
        ]
    },
    {
        "groupName": "Socials",
        "items":[
            {"name": "Reddit", "shortcutKey": "r", "url": "https://www.reddit.com/"},
            {"name": "Messenger", "shortcutKey": "M", "url": "https://www.messenger.com/t/2063582460419421"},
	    {"name": "Twitch", "shortcutKey": "t", "url": "https://www.twitch.tv/"},
	    {"name": "Youtube", "shortcutKey": "y", "url": "https://www.youtube.com/"},
 
            {"name": "Instagram", "shortcutKey": "i", "url": "https://www.instagram.com/"}
        ]
    },
    {
        "groupName": "Games",
        "items":[
            {"name": "Lutris", "shortcutKey": "l", "url": "https://lutris.net/"},
            {"name": "Proton", "shortcutKey": "p", "url": "https://www.protondb.com/"},
	    {"name": "LolChess", "shortcutKey": "L", "url": "https://lolchess.gg/profile/eune/auroradraco"},
	    {"name": "Discord", "shortcutKey": "d", "url": "https://discord.com/channels/@me"},

            {"name": "Dod", "shortcutKey": "D", "url": "https://www.dod.gr/"}
        ]
    }
]

let $container = document.getElementById("content");
let getUrl = {};

let $shortcutDisplayList = document.getElementsByClassName("shortcut");
let listeningForShortcut = false;
let listenerTimeout;

function setupWelcomeMessage(){
    let curHours = new Date().getHours();
    curHours = Math.floor(curHours/6); // Simply dividing current hours by 6 proves to be a good enough aproximation.
    if (curHours == 4) curHours = 3;
    let welcome = "Good " + WELCOME_MESSAGE_TEMPLATE[curHours] + ", " + NAME;
    document.getElementById("welcome-string").innerHTML = welcome;
}

function setupGroups(){
    for (let i = 0; i < MASTER_MAP.length; i++){
        let curGroupData = MASTER_MAP[i];

        let group = document.createElement("div");
        group.className = "group";
        $container.appendChild(group);

        let header = document.createElement("h1");
        header.innerHTML = curGroupData.groupName;
        group.appendChild(header);

        for (let j = 0; j < curGroupData.items.length; j++){
            let curItemData = curGroupData.items[j];

            let pContainer = document.createElement("p");
            group.appendChild(pContainer);

            let link = document.createElement("a");
            link.innerHTML = curItemData.name;
            link.setAttribute("href", curItemData.url);
            pContainer.appendChild(link);

            let shortcutDisplay = document.createElement("span");
            shortcutDisplay.innerHTML = curItemData.shortcutKey;
            shortcutDisplay.className = "shortcut";
            shortcutDisplay.style.animation = "none";
            pContainer.appendChild(shortcutDisplay);

            getUrl[curItemData.shortcutKey] = curItemData.url
        }
    }
}

function shortcutListener(e) {
    let key = e.key.toLowerCase();

    if (listeningForShortcut && getUrl.hasOwnProperty(key)){
        window.location = getUrl[key];
    }

    if (key === SHORTCUT_STARTER) {
        clearTimeout(listenerTimeout);
        listeningForShortcut = true;

        // Animation reset
        for (let i = 0; i < $shortcutDisplayList.length; i++){
            $shortcutDisplayList[i].style.animation = "none";
            setTimeout(function() { $shortcutDisplayList[i].style.animation = ''; }, 10);
        }

        listenerTimeout = setTimeout(function(){ listeningForShortcut = false; }, SHORTCUT_TIMEOUT);
    }
}

function main(){
    setupWelcomeMessage();
    setupGroups();
    document.addEventListener('keyup', shortcutListener, false);
}

main();
