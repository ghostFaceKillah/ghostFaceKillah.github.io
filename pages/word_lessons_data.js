// Hand-written mini lessons for Integrated Chinese function words — usage
// notes surfaced on the SRS word cards (pages/srs/registry.js). Same shape
// as the `tone`/`lesson` fields on colloquial entries (colloquial_data.js):
//   { tone?: "…", lesson: { intro?: "…", uses: [{ h?, hPy?, g, ex?, exPy?, exEn? }] } }
// Keyed by the word exactly as it appears in WORDS_DATA (`w`) — the registry
// refuses to load if a key here matches no word, so typos can't rot silently.
// A key that is also a colloquial character wins over the colloquial lesson.
//
// Scope: the words that carry a grammar system on their back — question-or vs
// statement-or, the but-family, if/because/so pairs, and the expectation
// adverbs (才/就, 再/又) whose partners 就 and 又 already have colloquial
// lessons pointing here.
window.WORD_LESSONS = {

  "还是": {
    lesson: {
      intro: "The question “or” — and the polite way to settle on a plan.",
      uses: [
        {
          h: "A 还是 B？", hPy: "A háishì B?",
          g: "“or” when asking someone to choose — the choice itself makes it a question, no 吗 needed",
          ex: "你喝茶还是喝咖啡？", exPy: "nǐ hē chá hái shì hē kā fēi?", exEn: "Do you want tea or coffee?",
        },
        {
          g: "“or” in a statement is 或者 instead — if nobody has to pick, don't use 还是",
          ex: "咖啡或者茶都可以。", exPy: "kā fēi huò zhě chá dōu kě yǐ.", exEn: "Coffee or tea — either is fine.",
        },
        {
          h: "还是……吧", hPy: "háishì … ba",
          g: "“had better / let's just” — after weighing the options, this one wins",
          ex: "太晚了，我们还是明天去吧。", exPy: "tài wǎn le, wǒ men hái shì míng tiān qù ba.", exEn: "It's late — let's just go tomorrow.",
        },
        {
          g: "“still” — like plain 还: the situation hasn't changed",
          ex: "他还是不明白。", exPy: "tā hái shì bù míng bái.", exEn: "He still doesn't understand.",
        },
      ],
    },
  },

  "或者": {
    lesson: {
      intro: "“Or” for statements — when nobody is being asked to choose.",
      uses: [
        {
          g: "“or” between options in a statement: either is acceptable",
          ex: "我们可以今天去或者明天去。", exPy: "wǒ men kě yǐ jīn tiān qù huò zhě míng tiān qù.", exEn: "We can go today or tomorrow.",
        },
        {
          h: "或者……，或者……", hPy: "huòzhě …, huòzhě …",
          g: "“either … or …”",
          ex: "或者你来，或者我去，都可以。", exPy: "huò zhě nǐ lái, huò zhě wǒ qù, dōu kě yǐ.", exEn: "Either you come over or I go — both fine.",
        },
        {
          g: "the test: a question asking someone to pick uses 还是; everything else uses 或者",
        },
      ],
    },
  },

  "可是": {
    lesson: {
      intro: "“But” — the everyday spoken one.",
      uses: [
        {
          g: "links two clauses: “but, however”",
          ex: "我很想去，可是我没有时间。", exPy: "wǒ hěn xiǎng qù, kě shì wǒ méi yǒu shí jiān.", exEn: "I really want to go, but I don't have time.",
        },
        {
          h: "虽然……，可是……", hPy: "suīrán …, kěshì …",
          g: "“although …, (but) …” — unlike English, Chinese keeps both halves",
          ex: "虽然有点儿贵，可是很好吃。", exPy: "suī rán yǒu diǎnr guì, kě shì hěn hǎo chī.", exEn: "A bit pricey, but delicious.",
        },
        {
          g: "vs 但是: same meaning — 可是 sounds a little softer and more spoken; swapping them is never wrong",
        },
      ],
    },
  },

  "但是": {
    lesson: {
      intro: "“But” — 可是's slightly more formal twin.",
      uses: [
        {
          g: "“but, however” — anywhere 可是 fits, 但是 fits too; it leans a little more written and emphatic",
          ex: "中文很难，但是很有意思。", exPy: "zhōng wén hěn nán, dàn shì hěn yǒu yì si.", exEn: "Chinese is hard, but really interesting.",
        },
        {
          h: "虽然……，但是……", hPy: "suīrán …, dànshì …",
          g: "the classic pair: “although …, still …”",
          ex: "虽然下雨，但是我们还是去了。", exPy: "suī rán xià yǔ, dàn shì wǒ men hái shì qù le.", exEn: "Although it was raining, we still went.",
        },
        {
          g: "in writing it often shrinks to just 但",
        },
      ],
    },
  },

  "要是": {
    lesson: {
      intro: "“If” — the spoken one.",
      uses: [
        {
          h: "要是……，就……", hPy: "yàoshi …, jiù …",
          g: "“if …, then …” — 就 opens the result clause",
          ex: "要是下雨，我们就不去了。", exPy: "yào shì xià yǔ, wǒ men jiù bú qù le.", exEn: "If it rains, we won't go.",
        },
        {
          h: "要是……的话", hPy: "yàoshi … dehuà",
          g: "an optional 的话 closes off the if-part — extra colloquial padding",
          ex: "要是你有时间的话，来我家玩儿吧。", exPy: "yào shì nǐ yǒu shí jiān de huà, lái wǒ jiā wánr ba.", exEn: "If you have time, come hang out at my place.",
        },
        {
          g: "vs 如果: same meaning and same 就 — 要是 is chattier, 如果 more neutral/written",
        },
      ],
    },
  },

  "因为": {
    tone: "为 reads wèi (4th tone) in 因为 and 为什么; it reads wéi (2nd tone) when it means “to be, to act as”.",
    lesson: {
      intro: "“Because” — almost always half of a pair.",
      uses: [
        {
          h: "因为……，所以……", hPy: "yīnwèi …, suǒyǐ …",
          g: "“because …, (so) …” — Chinese states the cause AND opens the result with 所以, unlike English",
          ex: "因为他病了，所以没来上课。", exPy: "yīn wèi tā bìng le, suǒ yǐ méi lái shàng kè.", exEn: "Because he was sick, he didn't come to class.",
        },
        {
          g: "answering 为什么 (“why?”), start with 因为",
          ex: "你为什么学中文？——因为很有意思。", exPy: "nǐ wèi shén me xué zhōng wén? — yīn wèi hěn yǒu yì si.", exEn: "Why do you study Chinese? — Because it's fun.",
        },
      ],
    },
  },

  "所以": {
    lesson: {
      intro: "“So, therefore” — the answer half of 因为.",
      uses: [
        {
          h: "因为……，所以……", hPy: "yīnwèi …, suǒyǐ …",
          g: "cause first, then 所以 + result — keep both words, unlike English “because/so”",
          ex: "因为太贵了，所以我没买。", exPy: "yīn wèi tài guì le, suǒ yǐ wǒ méi mǎi.", exEn: "It was too expensive, so I didn't buy it.",
        },
        {
          g: "alone at the start of a sentence: “so …” — drawing a conclusion",
          ex: "所以你明天不来了吗？", exPy: "suǒ yǐ nǐ míng tiān bù lái le ma?", exEn: "So you're not coming tomorrow?",
        },
      ],
    },
  },

  "才": {
    lesson: {
      intro: "“Not until / only then” — the mirror image of 就.",
      uses: [
        {
          g: "later or slower than expected",
          ex: "他昨天晚上十二点才睡觉。", exPy: "tā zuó tiān wǎn shàng shí èr diǎn cái shuì jiào.", exEn: "He didn't go to bed until midnight last night.",
        },
        {
          g: "vs 就: 就 = earlier than expected, 才 = later than expected",
          ex: "我六点就起床了，他十点才起床。", exPy: "wǒ liù diǎn jiù qǐ chuáng le, tā shí diǎn cái qǐ chuáng.", exEn: "I was already up at six; he didn't get up till ten.",
        },
        {
          g: "no 了 at the end of a 才 sentence — 我六点就来了, but 他十点才来",
        },
        {
          g: "“only, merely” with amounts",
          ex: "我才学了一年中文。", exPy: "wǒ cái xué le yì nián zhōng wén.", exEn: "I've only studied Chinese for a year.",
        },
      ],
    },
  },

  "再": {
    lesson: {
      intro: "“Again” — but only for repeats that haven't happened yet.",
      uses: [
        {
          g: "“again” in the future: requests, invitations, plans",
          ex: "请再说一遍。", exPy: "qǐng zài shuō yí biàn.", exEn: "Please say it once more.",
        },
        {
          g: "vs 又: a repeat that already happened is 又 — 再 is for repeats still to come",
          ex: "他昨天又来了，说明天再来。", exPy: "tā zuó tiān yòu lái le, shuō míng tiān zài lái.", exEn: "He came again yesterday, and said he'll come again tomorrow.",
        },
        {
          h: "先……，再……", hPy: "xiān …, zài …",
          g: "“first …, then …” — ordering steps",
          ex: "我们先吃饭，再看电影。", exPy: "wǒ men xiān chī fàn, zài kàn diàn yǐng.", exEn: "Let's eat first, then see the movie.",
        },
        {
          g: "hiding in 再见: literally “see (you) again”",
        },
      ],
    },
  },

  "别": {
    lesson: {
      intro: "One word, two faces: “don't!” and “other”.",
      uses: [
        {
          h: "别 + verb", hPy: "bié + verb",
          g: "“don't …” — quick negative command",
          ex: "别忘了带钱。", exPy: "bié wàng le dài qián.", exEn: "Don't forget to bring money.",
        },
        {
          h: "别……了", hPy: "bié … le",
          g: "“stop …ing” — the 了 says the thing is already happening",
          ex: "别说话了，电影开始了。", exPy: "bié shuō huà le, diàn yǐng kāi shǐ le.", exEn: "Stop talking — the movie's starting.",
        },
        {
          h: "别的", hPy: "bié de",
          g: "“other, else”",
          ex: "你还要别的吗？", exPy: "nǐ hái yào bié de ma?", exEn: "Anything else?",
        },
      ],
    },
  },

  "跟": {
    lesson: {
      intro: "“With” — and the glue in “together with”.",
      uses: [
        {
          h: "跟……一起", hPy: "gēn … yìqǐ",
          g: "“together with …” — the with-phrase comes BEFORE the verb",
          ex: "我想跟你一起去。", exPy: "wǒ xiǎng gēn nǐ yì qǐ qù.", exEn: "I want to go with you.",
        },
        {
          h: "跟……说", hPy: "gēn … shuō",
          g: "“tell / talk to …” — speaking verbs take 跟",
          ex: "别跟他说！", exPy: "bié gēn tā shuō!", exEn: "Don't tell him!",
        },
        {
          g: "“and” between people, in speech",
          ex: "我跟他是同学。", exPy: "wǒ gēn tā shì tóng xué.", exEn: "He and I are classmates.",
        },
        {
          h: "跟我来", hPy: "gēn wǒ lái",
          g: "“follow me” — the root meaning of 跟",
        },
      ],
    },
  },

  "让": {
    lesson: {
      intro: "“Let / make someone do something” — the little pivot verb.",
      uses: [
        {
          h: "让 + person + verb", hPy: "ràng + person + verb",
          g: "“let / have / make sb do sth” — the person sits in the middle",
          ex: "妈妈不让我玩儿电脑。", exPy: "mā ma bú ràng wǒ wánr diàn nǎo.", exEn: "Mom won't let me play on the computer.",
        },
        {
          g: "“make sb feel …”",
          ex: "这件事让我很高兴。", exPy: "zhè jiàn shì ràng wǒ hěn gāo xìng.", exEn: "This made me really happy.",
        },
        {
          h: "让一下", hPy: "ràng yíxià",
          g: "“excuse me, coming through” — asking someone to make way",
          ex: "请让一下。", exPy: "qǐng ràng yí xià.", exEn: "Excuse me, could I get past?",
        },
      ],
    },
  },

};
