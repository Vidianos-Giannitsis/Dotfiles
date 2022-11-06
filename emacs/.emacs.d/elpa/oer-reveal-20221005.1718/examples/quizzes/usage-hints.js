// SPDX-FileCopyrightText: 2019 Jens Lechtenbörger
// SPDX-License-Identifier: CC-BY-SA-4.0

quizUsageHints = {
    "info": {
        "name":    "", // Should be empty with emacs-reveal
        "main":    "How do these presentations work?",
        "level1":  "Excellent!",     // 80-100%
        "level2":  "Please re-try.", // 60-79%
        "level3":  "Please re-try.", // 40-59%
        "level4":  "Maybe ask for help?",       // 20-39%
        "level5":  "Please try the quiz again." // 0-19%, no comma here
    },
    "questions": [
	{
            "q": "Did you read the usage hints?",
            "a": [
                {"option": "Yes, I'm surprised by the amount of non-obvious features.", "correct": true},
                {"option": "No.", "correct": false}
            ],
            "correct": "<p><span>Very good!  You should be in pretty good shape.</span></p>",
            "incorrect": "<p>Please check out <a href=\"https://oer.gitlab.io/hints.html\">the usage hints</a>.</p>" // no comma here
        }
    ]
};
