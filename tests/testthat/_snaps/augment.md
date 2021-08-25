# augment.gam works as expected with gaussian models

    WAoAAAACAAQAAwACAwAAAAMTAAAABwAAAA4AAACWQAwAAAAAAABACAAAAAAAAEAJmZmZmZma
    QAjMzMzMzM1ADMzMzMzMzUAPMzMzMzMzQAszMzMzMzNACzMzMzMzM0AHMzMzMzMzQAjMzMzM
    zM1ADZmZmZmZmkALMzMzMzMzQAgAAAAAAABACAAAAAAAAEAQAAAAAAAAQBGZmZmZmZpADzMz
    MzMzM0AMAAAAAAAAQA5mZmZmZmZADmZmZmZmZkALMzMzMzMzQA2ZmZmZmZpADMzMzMzMzUAK
    ZmZmZmZmQAszMzMzMzNACAAAAAAAAEALMzMzMzMzQAwAAAAAAABACzMzMzMzM0AJmZmZmZma
    QAjMzMzMzM1ACzMzMzMzM0AQZmZmZmZmQBDMzMzMzM1ACMzMzMzMzUAJmZmZmZmaQAwAAAAA
    AABADMzMzMzMzUAIAAAAAAAAQAszMzMzMzNADAAAAAAAAEACZmZmZmZmQAmZmZmZmZpADAAA
    AAAAAEAOZmZmZmZmQAgAAAAAAABADmZmZmZmZkAJmZmZmZmaQA2ZmZmZmZpACmZmZmZmZkAJ
    mZmZmZmaQAmZmZmZmZpACMzMzMzMzUACZmZmZmZmQAZmZmZmZmZABmZmZmZmZkAKZmZmZmZm
    QAMzMzMzMzNABzMzMzMzM0AFmZmZmZmaQAAAAAAAAABACAAAAAAAAEABmZmZmZmaQAczMzMz
    MzNABzMzMzMzM0AIzMzMzMzNQAgAAAAAAABABZmZmZmZmkABmZmZmZmaQAQAAAAAAABACZmZ
    mZmZmkAGZmZmZmZmQAQAAAAAAABABmZmZmZmZkAHMzMzMzMzQAgAAAAAAABABmZmZmZmZkAI
    AAAAAAAAQAczMzMzMzNABMzMzMzMzUADMzMzMzMzQAMzMzMzMzNABZmZmZmZmkAFmZmZmZma
    QAgAAAAAAABACzMzMzMzM0AIzMzMzMzNQAJmZmZmZmZACAAAAAAAAEAEAAAAAAAAQATMzMzM
    zM1ACAAAAAAAAEAEzMzMzMzNQAJmZmZmZmZABZmZmZmZmkAIAAAAAAAAQAczMzMzMzNABzMz
    MzMzM0AEAAAAAAAAQAZmZmZmZmZACmZmZmZmZkAFmZmZmZmaQAgAAAAAAABABzMzMzMzM0AI
    AAAAAAAAQAgAAAAAAABABAAAAAAAAEAHMzMzMzMzQAQAAAAAAABADMzMzMzMzUAJmZmZmZma
    QAWZmZmZmZpACAAAAAAAAEAEAAAAAAAAQAZmZmZmZmZACZmZmZmZmkAIAAAAAAAAQA5mZmZm
    ZmZABMzMzMzMzUABmZmZmZmaQAmZmZmZmZpABmZmZmZmZkAGZmZmZmZmQAWZmZmZmZpACmZm
    ZmZmZkAJmZmZmZmaQAZmZmZmZmZACAAAAAAAAEAGZmZmZmZmQAgAAAAAAABABmZmZmZmZkAO
    ZmZmZmZmQAZmZmZmZmZABmZmZmZmZkAEzMzMzMzNQAgAAAAAAABACzMzMzMzM0AIzMzMzMzN
    QAgAAAAAAABACMzMzMzMzUAIzMzMzMzNQAjMzMzMzM1ABZmZmZmZmkAJmZmZmZmaQApmZmZm
    ZmZACAAAAAAAAEAEAAAAAAAAQAgAAAAAAABACzMzMzMzM0AIAAAAAAAAAAAADgAAAJY/9mZm
    ZmZmZj/2ZmZmZmZmP/TMzMzMzM0/+AAAAAAAAD/2ZmZmZmZmP/szMzMzMzM/9mZmZmZmZj/4
    AAAAAAAAP/ZmZmZmZmY/+AAAAAAAAD/4AAAAAAAAP/mZmZmZmZo/9mZmZmZmZj/xmZmZmZma
    P/MzMzMzMzM/+AAAAAAAAD/0zMzMzMzNP/ZmZmZmZmY/+zMzMzMzMz/4AAAAAAAAP/szMzMz
    MzM/+AAAAAAAAD/wAAAAAAAAP/szMzMzMzM//mZmZmZmZj/5mZmZmZmaP/mZmZmZmZo/+AAA
    AAAAAD/2ZmZmZmZmP/mZmZmZmZo/+ZmZmZmZmj/4AAAAAAAAP/gAAAAAAAA/9mZmZmZmZj/4
    AAAAAAAAP/MzMzMzMzM/9MzMzMzMzT/2ZmZmZmZmP/TMzMzMzM0/+AAAAAAAAD/0zMzMzMzN
    P/TMzMzMzM0/9MzMzMzMzT/5mZmZmZmaP/5mZmZmZmY/9mZmZmZmZj/5mZmZmZmaP/ZmZmZm
    ZmY/+AAAAAAAAD/2ZmZmZmZmQBLMzMzMzM1AEgAAAAAAAEATmZmZmZmaQBAAAAAAAABAEmZm
    ZmZmZkASAAAAAAAAQBLMzMzMzM1ACmZmZmZmZkASZmZmZmZmQA8zMzMzMzNADAAAAAAAAEAQ
    zMzMzMzNQBAAAAAAAABAEszMzMzMzUAMzMzMzMzNQBGZmZmZmZpAEgAAAAAAAEAQZmZmZmZm
    QBIAAAAAAABADzMzMzMzM0ATMzMzMzMzQBAAAAAAAABAE5mZmZmZmkASzMzMzMzNQBEzMzMz
    MzNAEZmZmZmZmkATMzMzMzMzQBQAAAAAAABAEgAAAAAAAEAMAAAAAAAAQA5mZmZmZmZADZmZ
    mZmZmkAPMzMzMzMzQBRmZmZmZmZAEgAAAAAAAEASAAAAAAAAQBLMzMzMzM1AEZmZmZmZmkAQ
    ZmZmZmZmQBAAAAAAAABAEZmZmZmZmkASZmZmZmZmQBAAAAAAAABACmZmZmZmZkAQzMzMzMzN
    QBDMzMzMzM1AEMzMzMzMzUARMzMzMzMzQAgAAAAAAABAEGZmZmZmZkAYAAAAAAAAQBRmZmZm
    ZmZAF5mZmZmZmkAWZmZmZmZmQBczMzMzMzNAGmZmZmZmZkASAAAAAAAAQBkzMzMzMzNAFzMz
    MzMzM0AYZmZmZmZmQBRmZmZmZmZAFTMzMzMzM0AWAAAAAAAAQBQAAAAAAABAFGZmZmZmZkAV
    MzMzMzMzQBYAAAAAAABAGszMzMzMzUAbmZmZmZmaQBQAAAAAAABAFszMzMzMzUATmZmZmZma
    QBrMzMzMzM1AE5mZmZmZmkAWzMzMzMzNQBgAAAAAAABAEzMzMzMzM0ATmZmZmZmaQBZmZmZm
    ZmZAFzMzMzMzM0AYZmZmZmZmQBmZmZmZmZpAFmZmZmZmZkAUZmZmZmZmQBZmZmZmZmZAGGZm
    ZmZmZkAWZmZmZmZmQBYAAAAAAABAEzMzMzMzM0AVmZmZmZmaQBZmZmZmZmZAFGZmZmZmZkAU
    ZmZmZmZmQBeZmZmZmZpAFszMzMzMzUAUzMzMzMzNQBQAAAAAAABAFMzMzMzMzUAVmZmZmZma
    QBRmZmZmZmYAAAAOAAAAlkALUpjIynXNQAtSmMjKdc1ACxGUZtYB2kALivBXem/sQAtSmMjK
    dc1AC8ZI9Lh2FUALUpjIynXNQAuK8Fd6b+xAC1KYyMp1zUALivBXem/sQAuK8Fd6b+xAC7Pr
    PIbsw0ALUpjIynXNQAqHHkdlEaJACszTKISuREALivBXem/sQAsRlGbWAdpAC1KYyMp1zUAL
    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
    AAAKZGF0YS5mcmFtZQAAAP4=

# augment.gam works as expected with binomial models

    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
    AAAKZGF0YS5mcmFtZQAAAP4=
