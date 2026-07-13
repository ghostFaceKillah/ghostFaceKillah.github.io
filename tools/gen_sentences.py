# -*- coding: utf-8 -*-
"""
Generate the sentence data for the HSK Sentence Trainer.

Sentences (Chinese + English) are hand-authored below to stay within
HSK 1 / HSK 2 vocabulary and grammar. Pinyin is generated automatically
with pypinyin so the tone marks are accurate and consistent.

Run:  python3 tools/gen_sentences.py
Out:  pages/sentences_data.js
"""

import json
import os
import re

from pypinyin import pinyin, Style

HERE = os.path.dirname(os.path.abspath(__file__))
OUT = os.path.join(HERE, "..", "pages", "sentences_data.js")

HANZI = re.compile(r"[一-鿿]+")
PUNCT_MAP = {"，": ",", "。": ".", "？": "?", "！": "!", "：": ":",
             "、": ",", "；": ";", "…": "...", "—": "-"}

_T1, _T2, _T3, _T4 = "āēīōūǖ", "áéíóúǘ", "ǎěǐǒǔǚ", "àèìòùǜ"


def _tone(syl: str) -> int:
    for marks, t in ((_T1, 1), (_T2, 2), (_T3, 3), (_T4, 4)):
        if any(c in syl for c in marks):
            return t
    return 5  # neutral


def _aligned(zh):
    """Return list of (char, syllable_or_None). Pinyin is generated per
    contiguous Han run so phrase context is preserved, then aligned 1:1."""
    pairs = []
    i = 0
    for m in HANZI.finditer(zh):
        for ch in zh[i:m.start()]:
            pairs.append((ch, None))
        run = m.group()
        sylls = [p[0] for p in pinyin(run, style=Style.TONE)]
        for ch, s in zip(run, sylls):
            pairs.append((ch, s))
        i = m.end()
    for ch in zh[i:]:
        pairs.append((ch, None))
    return pairs


def _next_syllable(pairs, idx):
    for ch, s in pairs[idx + 1:]:
        if s is not None:
            return s
    return None


def _corrected(zh: str):
    """Aligned (char, syllable) pairs after polyphone corrections and the
    written 不/一 tone-sandhi adjustments."""
    pairs = _aligned(zh)

    # --- polyphone corrections ---
    for idx, (ch, s) in enumerate(pairs):
        if s is None:
            continue
        prev_ch = pairs[idx - 1][0] if idx > 0 else ""
        next_ch = pairs[idx + 1][0] if idx + 1 < len(pairs) else ""
        if ch == "长":                       # 长得 = "grow/look", otherwise "long"
            s = "zhǎng" if next_ch == "得" else "cháng"
        elif ch == "都" and s == "dū" and prev_ch != "首":  # 都 = "all"; keep 首都
            s = "dōu"
        elif ch == "子" and s in ("zǐ", "zī"):  # noun suffix 子 = neutral tone
            s = "zi"
        elif ch == "好" and s == "hào":       # 好好 etc., never the "fond of" reading
            s = "hǎo"
        elif ch == "空" and (prev_ch == "有" or next_ch == "儿"):
            s = "kòng"                        # 有空(儿) = free time
        elif ch == "教" and next_ch in ("我", "你", "他", "她"):
            s = "jiāo"                        # verb "teach" (教我们), not 教室
        elif ch == "得":                      # 得 as a complement marker = neutral "de"
            if next_ch in ("走", "请", "学", "赶", "买") and prev_ch != "觉":
                s = "děi"                     # modal 得 = "must" (我得走, 得请我…)
            else:
                s = "de"
        pairs[idx] = (ch, s)

    # --- 不 / 一 tone sandhi ---
    for idx, (ch, s) in enumerate(pairs):
        if ch == "不":
            nxt = _next_syllable(pairs, idx)
            s = "bú" if (nxt and _tone(nxt) == 4) else "bù"
        elif ch == "一":
            prev = pairs[idx - 1][0] if idx > 0 else ""
            nxt_ch = ""
            for c2, s2 in pairs[idx + 1:]:
                if s2 is not None:
                    nxt_ch = c2
                    break
            nxt = _next_syllable(pairs, idx)
            if prev in ("第", "期") or nxt_ch in ("月", "号") or nxt is None:
                s = "yī"                      # ordinal / date / standalone
            else:
                s = "yí" if _tone(nxt) == 4 else "yì"
        else:
            continue
        pairs[idx] = (ch, s)

    return pairs


def to_pinyin(zh: str) -> str:
    """Tone-marked pinyin with correct polyphone readings, 不/一 tone
    sandhi and erhua. Word-spaced, punctuation kept inline."""
    pairs = _corrected(zh)

    # --- build output, merging erhua ---
    toks = []
    for idx, (ch, s) in enumerate(pairs):
        if ch == "儿" and s is not None:
            prev_ch = pairs[idx - 1][0] if idx > 0 else ""
            next_ch = pairs[idx + 1][0] if idx + 1 < len(pairs) else ""
            if prev_ch == "女" or next_ch == "子":   # syllabic 儿 (女儿, 儿子)
                toks.append(s)
            elif toks and re.search(r"[a-zü]", toks[-1]):
                toks[-1] = toks[-1] + "r"            # erhua: 点儿 -> diǎnr
            else:
                toks.append(s)
            continue
        if s is None:
            toks.append(PUNCT_MAP.get(ch, ch))
        else:
            toks.append(s)

    out = ""
    for t in toks:
        if t and all(c in ",.?!:;-." or c == "." for c in t):
            out = out.rstrip() + t + " "
        else:
            out += t + " "
    text = re.sub(r"\s+", " ", out).strip()

    # fixed expressions where 不 is conventionally toneless
    text = text.replace("duì bù qǐ", "duì bu qǐ")
    return text


# ===================== Polyphone / tone-change notes =====================
# A sentence gets short notes when a reading rule actually plays out in it:
# the 一 and 不 tone sandhi, and polyphones whose other reading a learner
# is likely to know (觉, 得, 长, 乐, 都, 空, 教, 便).  Each note states the
# rule and how it applies in this sentence.

# (char, syllable used here) -> note.  Only readings that are easy to get
# wrong are listed; the everyday reading of frequent particles (e.g. the
# complement 得 = de) is deliberately not flagged on every card.
POLY_NOTES = {
    ("觉", "jiào"): "Polyphone: 觉 is jiào in 睡觉 (to sleep), but jué in 觉得 (to feel / think).",
    ("觉", "jué"):  "Polyphone: 觉 is jué in 觉得 (to feel / think), but jiào in 睡觉 (to sleep).",
    ("得", "děi"):  "Polyphone: 得 is děi when it means \"must / have to\"; as the verb complement particle (说得很好) it is neutral de.",
    # 长 = cháng ("long") is unambiguous in context, so only zhǎng is flagged.
    ("长", "zhǎng"): "Polyphone: 长 is zhǎng in 长得 (\"to grow / look\"); when it means \"long\" it is cháng.",
    ("乐", "lè"):   "Polyphone: 乐 is lè when it means \"happy\" (快乐, 可乐), but yuè in 音乐 (music).",
    ("乐", "yuè"):  "Polyphone: 乐 is yuè in 音乐 (music), but lè when it means \"happy\" (快乐, 可乐).",
    ("都", "dū"):   "Polyphone: 都 is dū in 首都 (capital), but dōu when it means \"all / both\".",
    ("空", "kòng"): "Polyphone: 空 is kòng when it means \"free time\" (有空儿), but kōng for \"empty / air\" (航空).",
    ("空", "kōng"): "Polyphone: 空 is kōng in 航空 (aviation, \"air\"), but kòng when it means \"free time\" (有空儿).",
    ("教", "jiāo"): "Polyphone: 教 is jiāo as the verb \"to teach\", but jiào in 教室 (classroom).",
    ("教", "jiào"): "Polyphone: 教 is jiào in 教室 (classroom), but jiāo as the verb \"to teach\".",
    ("便", "pián"): "Polyphone: 便 is pián in 便宜 (cheap), but biàn in 方便 (convenient).",
    ("便", "biàn"): "Polyphone: 便 is biàn in 方便 (convenient), but pián in 便宜 (cheap).",
}

def _next_hanzi(pairs, idx):
    """First (char, syllable) after idx that is an actual Han syllable."""
    for ch, s in pairs[idx + 1:]:
        if s is not None:
            return ch, s
    return "", ""


def polyphone_notes(zh: str):
    """Ordered, de-duplicated notes for the rules at play in this sentence."""
    pairs = _corrected(zh)
    notes = []

    def add(note):
        if note and note not in notes:
            notes.append(note)

    if "对不起" in zh:
        add("Tone change: in 对不起 the 不 is toneless — duì bu qǐ.")

    for idx, (ch, s) in enumerate(pairs):
        if s is None:
            continue
        if ch == "一" and s in ("yí", "yì"):
            nxt_ch, nxt = _next_hanzi(pairs, idx)
            add("Tone change: 一 is yī alone or in numbers/dates, "
                "yí before a 4th tone, yì before 1st/2nd/3rd tones — "
                f"here 一{nxt_ch} is {s} {nxt}.")
        elif ch == "不" and s == "bú":
            nxt_ch, nxt = _next_hanzi(pairs, idx)
            add("Tone change: 不 becomes bú before a 4th tone, otherwise bù — "
                f"here 不{nxt_ch} is bú {nxt}.")
        elif (ch, s) in POLY_NOTES:
            add(POLY_NOTES[(ch, s)])

    return notes


# Each entry: (chinese, english)

HSK1 = []
HSK2 = []

# ===================== HSK 1 — 500 sentences =====================

# --- Greetings, names, identity ---
HSK1 += [
    ("你好！", "Hello!"),
    ("你好吗？", "How are you?"),
    ("我很好，谢谢。", "I'm very well, thank you."),
    ("你叫什么名字？", "What is your name?"),
    ("我叫李明。", "My name is Li Ming."),
    ("他叫什么名字？", "What is his name?"),
    ("她的名字很漂亮。", "Her name is very pretty."),
    ("认识你很高兴。", "Nice to meet you."),
    ("这是我的朋友。", "This is my friend."),
    ("那是我的老师。", "That is my teacher."),
    ("我是学生。", "I am a student."),
    ("你是老师吗？", "Are you a teacher?"),
    ("我不是老师。", "I am not a teacher."),
    ("他是医生。", "He is a doctor."),
    ("她是我的同学。", "She is my classmate."),
    ("我们都是中国人。", "We are all Chinese."),
    ("你是哪国人？", "What country are you from?"),
    ("我是中国人。", "I am Chinese."),
    ("谢谢你！", "Thank you!"),
    ("不客气。", "You're welcome."),
    ("对不起。", "I'm sorry."),
    ("没关系。", "It's okay."),
    ("请坐。", "Please sit down."),
    ("请喝茶。", "Please drink some tea."),
    ("再见！", "Goodbye!"),
    ("明天见。", "See you tomorrow."),
    ("老师好！", "Hello, teacher!"),
    ("同学们好！", "Hello, classmates!"),
    ("你认识他吗？", "Do you know him?"),
    ("我不认识她。", "I don't know her."),
    ("他是谁？", "Who is he?"),
    ("她是我的妈妈。", "She is my mother."),
    ("这是什么？", "What is this?"),
    ("这是一本书。", "This is a book."),
    ("那是你的电脑吗？", "Is that your computer?"),
    ("是的，那是我的电脑。", "Yes, that is my computer."),
    ("你会说汉语吗？", "Can you speak Chinese?"),
    ("我会说一点儿汉语。", "I can speak a little Chinese."),
    ("请说汉语。", "Please speak Chinese."),
    ("我喜欢学习汉语。", "I like studying Chinese."),
]

# --- Family ---
HSK1 += [
    ("这是我的爸爸。", "This is my father."),
    ("我爱我的妈妈。", "I love my mother."),
    ("我有一个儿子。", "I have a son."),
    ("我有一个女儿。", "I have a daughter."),
    ("我家有四个人。", "There are four people in my family."),
    ("你家有几个人？", "How many people are in your family?"),
    ("我的爸爸是医生。", "My father is a doctor."),
    ("我的妈妈是老师。", "My mother is a teacher."),
    ("他的儿子很小。", "His son is very young."),
    ("她的女儿很漂亮。", "Her daughter is very pretty."),
    ("我爸爸今天工作。", "My father is working today."),
    ("我妈妈在家。", "My mother is at home."),
    ("我们爱我们的家。", "We love our family."),
    ("他有很多朋友。", "He has many friends."),
    ("我没有狗。", "I don't have a dog."),
    ("我有一个猫。", "I have a cat."),
    ("你有狗吗？", "Do you have a dog?"),
    ("我的朋友有一个小狗。", "My friend has a little dog."),
    ("我爸爸妈妈都很好。", "My mother and father are both well."),
    ("我儿子今年五岁。", "My son is five years old this year."),
]

# --- Numbers, age, counting, money ---
HSK1 += [
    ("一、二、三、四、五。", "One, two, three, four, five."),
    ("六、七、八、九、十。", "Six, seven, eight, nine, ten."),
    ("我有三本书。", "I have three books."),
    ("桌子上有两个苹果。", "There are two apples on the table."),
    ("你今年多大？", "How old are you this year?"),
    ("我今年二十岁。", "I am twenty years old this year."),
    ("她今年三十岁。", "She is thirty years old this year."),
    ("这本书多少钱？", "How much is this book?"),
    ("这本书十块钱。", "This book is ten yuan."),
    ("那个苹果多少钱？", "How much is that apple?"),
    ("一个苹果三块钱。", "One apple is three yuan."),
    ("我有一百块钱。", "I have one hundred yuan."),
    ("这些东西很多钱。", "These things cost a lot of money."),
    ("你有多少本书？", "How many books do you have?"),
    ("我有九本书。", "I have nine books."),
    ("教室里有多少个学生？", "How many students are in the classroom?"),
    ("我想买两个杯子。", "I want to buy two cups."),
    ("这里有几个人？", "How many people are here?"),
    ("这里有八个人。", "There are eight people here."),
    ("我买了五个苹果。", "I bought five apples."),
]

# --- Time: days, dates, clock ---
HSK1 += [
    ("今天是星期几？", "What day is it today?"),
    ("今天是星期一。", "Today is Monday."),
    ("明天是星期二。", "Tomorrow is Tuesday."),
    ("昨天是星期天。", "Yesterday was Sunday."),
    ("今天几号？", "What date is it today?"),
    ("今天是五月八号。", "Today is May 8th."),
    ("我的生日是九月十号。", "My birthday is September 10th."),
    ("现在几点了？", "What time is it now?"),
    ("现在三点。", "It's three o'clock now."),
    ("现在八点三十。", "It's eight thirty now."),
    ("我上午九点去学校。", "I go to school at nine in the morning."),
    ("我下午五点回家。", "I go home at five in the afternoon."),
    ("我中午十二点吃饭。", "I eat at twelve noon."),
    ("他每天晚上看电视。", "He watches TV every evening."),
    ("我昨天去了商店。", "I went to the store yesterday."),
    ("我明天想去看电影。", "I want to go see a movie tomorrow."),
    ("现在是几月？", "What month is it now?"),
    ("现在是六月。", "It is June now."),
    ("我们星期六去北京。", "We are going to Beijing on Saturday."),
    ("请等我五分钟。", "Please wait five minutes for me."),
    ("我们上午有汉语课。", "We have Chinese class in the morning."),
    ("他十点钟睡觉。", "He goes to sleep at ten o'clock."),
    ("今天上午我在家。", "I am at home this morning."),
    ("明天下午我去医院。", "I'm going to the hospital tomorrow afternoon."),
    ("你什么时候来？", "When are you coming?"),
]

# --- Likes, wants, feelings ---
HSK1 += [
    ("我喜欢喝茶。", "I like drinking tea."),
    ("我不喜欢喝水。", "I don't like drinking water."),
    ("你喜欢什么？", "What do you like?"),
    ("我喜欢看书。", "I like reading books."),
    ("她喜欢小猫。", "She likes little cats."),
    ("我想喝水。", "I want to drink water."),
    ("我想吃米饭。", "I want to eat rice."),
    ("我不想去。", "I don't want to go."),
    ("你想买什么？", "What do you want to buy?"),
    ("我想买一些水果。", "I want to buy some fruit."),
    ("我很高兴。", "I am very happy."),
    ("今天我很高兴。", "I am very happy today."),
    ("他喜欢这本书。", "He likes this book."),
    ("我们都喜欢老师。", "We all like the teacher."),
    ("你喜欢中国吗？", "Do you like China?"),
    ("我很喜欢中国。", "I like China very much."),
    ("我喜欢这个学校。", "I like this school."),
    ("妈妈喜欢漂亮的衣服。", "Mom likes pretty clothes."),
    ("我想看那个电影。", "I want to watch that movie."),
    ("他想认识你。", "He wants to get to know you."),
]

# --- Eating and drinking ---
HSK1 += [
    ("我想吃中国菜。", "I want to eat Chinese food."),
    ("你喜欢吃什么菜？", "What food do you like to eat?"),
    ("我们去饭店吃饭吧。", "Let's go to the restaurant to eat."),
    ("这个菜很好吃。", "This dish is very tasty."),
    ("我每天吃水果。", "I eat fruit every day."),
    ("妈妈做的菜很好吃。", "The food Mom makes is very tasty."),
    ("我想喝一杯茶。", "I want to drink a cup of tea."),
    ("请给我一杯水。", "Please give me a glass of water."),
    ("你喝茶还是喝水？", "Do you drink tea or water?"),
    ("我中午吃米饭。", "I eat rice at noon."),
    ("我们一起吃饭吧。", "Let's eat together."),
    ("他不喜欢吃苹果。", "He doesn't like eating apples."),
    ("这些水果很多。", "There is a lot of fruit here."),
    ("我买了一些菜。", "I bought some vegetables."),
    ("饭店里有很多人。", "There are many people in the restaurant."),
    ("你想吃什么？", "What do you want to eat?"),
    ("我想吃一点儿米饭。", "I want to eat a little rice."),
    ("茶太热了。", "The tea is too hot."),
    ("水太冷了。", "The water is too cold."),
    ("我们在饭店吃了很多东西。", "We ate a lot at the restaurant."),
]

# --- Daily activities: go, come, see, listen, read, write, buy ---
HSK1 += [
    ("我去学校。", "I am going to school."),
    ("你去哪儿？", "Where are you going?"),
    ("我去商店买东西。", "I'm going to the store to buy things."),
    ("他来我家。", "He is coming to my house."),
    ("请来我家坐坐。", "Please come and sit at my place."),
    ("我看见了一个小猫。", "I saw a little cat."),
    ("我喜欢看电视。", "I like watching TV."),
    ("妈妈在看电影。", "Mom is watching a movie."),
    ("我想听他说话。", "I want to listen to him talk."),
    ("老师，请听我说。", "Teacher, please listen to me."),
    ("我每天读书。", "I read books every day."),
    ("他在读汉语书。", "He is reading a Chinese book."),
    ("我会写我的名字。", "I can write my name."),
    ("请写你的名字。", "Please write your name."),
    ("我想买一件衣服。", "I want to buy a piece of clothing."),
    ("妈妈去商店买水果。", "Mom is going to the store to buy fruit."),
    ("我们回家吧。", "Let's go home."),
    ("他回学校了。", "He went back to school."),
    ("我现在很忙，不能去。", "I'm very busy now, I can't go."),
    ("你能来吗？", "Can you come?"),
    ("我不能去，对不起。", "I can't go, sorry."),
    ("我会做中国菜。", "I can make Chinese food."),
    ("你会开车吗？", "Can you drive?"),
    ("请打开门。", "Please open the door."),
    ("他在打电话。", "He is making a phone call."),
]

# --- Places and location ---
HSK1 += [
    ("你住在哪儿？", "Where do you live?"),
    ("我住在北京。", "I live in Beijing."),
    ("我的家在学校后面。", "My home is behind the school."),
    ("商店在饭店前面。", "The store is in front of the restaurant."),
    ("猫在桌子下面。", "The cat is under the table."),
    ("书在椅子上。", "The book is on the chair."),
    ("医院里有很多医生。", "There are many doctors in the hospital."),
    ("学校里有很多学生。", "There are many students in the school."),
    ("老师在教室里。", "The teacher is in the classroom."),
    ("妈妈在商店里。", "Mom is in the store."),
    ("我的电脑在桌子上。", "My computer is on the table."),
    ("你的书在哪儿？", "Where is your book?"),
    ("我的书在家里。", "My book is at home."),
    ("他在饭店工作。", "He works at a restaurant."),
    ("我在医院看见了她。", "I saw her at the hospital."),
    ("前面有一个商店。", "There is a store ahead."),
    ("后面有很多人。", "There are many people behind."),
    ("北京有很多人。", "Beijing has a lot of people."),
    ("我想去中国学习。", "I want to go to China to study."),
    ("我们的学校很大。", "Our school is very big."),
    ("这个饭店很大。", "This restaurant is very big."),
    ("那个商店很小。", "That store is very small."),
    ("我家有一个大桌子。", "My home has a big table."),
    ("教室里有几个桌子？", "How many tables are in the classroom?"),
    ("椅子上有一本书。", "There is a book on the chair."),
]

# --- Weather and seasons ---
HSK1 += [
    ("今天天气很好。", "The weather is very nice today."),
    ("今天很热。", "It is very hot today."),
    ("昨天很冷。", "It was very cold yesterday."),
    ("明天天气怎么样？", "What will the weather be like tomorrow?"),
    ("今天下雨。", "It is raining today."),
    ("明天会下雨吗？", "Will it rain tomorrow?"),
    ("外面在下雨。", "It is raining outside."),
    ("北京今天很冷。", "It is very cold in Beijing today."),
    ("我不喜欢热天。", "I don't like hot days."),
    ("下雨了，我不想去。", "It's raining, I don't want to go."),
    ("今天的天气不太好。", "The weather is not very good today."),
    ("现在天气很热。", "The weather is very hot now."),
    ("今天天气很好，我们去走走。", "The weather is nice today, let's go for a walk."),
    ("下午会下雨。", "It will rain in the afternoon."),
    ("天气冷的时候我不想去学校。", "When the weather is cold, I don't want to go to school."),
]

# --- Questions ---
HSK1 += [
    ("这是什么东西？", "What thing is this?"),
    ("那是谁的书？", "Whose book is that?"),
    ("你在做什么？", "What are you doing?"),
    ("你想去哪儿？", "Where do you want to go?"),
    ("你的老师是谁？", "Who is your teacher?"),
    ("现在是什么时候？", "What time is it now?"),
    ("你有多少钱？", "How much money do you have?"),
    ("这个怎么样？", "How about this one?"),
    ("这个菜怎么样？", "How is this dish?"),
    ("你怎么去学校？", "How do you go to school?"),
    ("你怎么了？", "What's wrong with you?"),
    ("哪个是你的？", "Which one is yours?"),
    ("哪本书是你的？", "Which book is yours?"),
    ("你喜欢哪个？", "Which one do you like?"),
    ("谁是你的老师？", "Who is your teacher?"),
    ("你想买几个？", "How many do you want to buy?"),
    ("你们学校有多少学生？", "How many students does your school have?"),
    ("他在哪儿工作？", "Where does he work?"),
    ("你今天怎么样？", "How are you today?"),
    ("这些是什么？", "What are these?"),
]

# --- Descriptions and adjectives ---
HSK1 += [
    ("这本书很大。", "This book is very big."),
    ("那个杯子很小。", "That cup is very small."),
    ("她很漂亮。", "She is very pretty."),
    ("这件衣服很漂亮。", "This piece of clothing is very pretty."),
    ("我们今天都很高兴。", "We are all very happy today."),
    ("这个东西太大了。", "This thing is too big."),
    ("这本书太小了。", "This book is too small."),
    ("我的猫很小。", "My cat is very small."),
    ("他的家很大。", "His home is very big."),
    ("这里人很多。", "There are a lot of people here."),
    ("那里人很少。", "There are few people there."),
    ("这个商店东西很多。", "This store has a lot of things."),
    ("这杯茶太热了。", "This cup of tea is too hot."),
    ("天气太冷了。", "The weather is too cold."),
    ("他的电脑很好。", "His computer is very good."),
    ("这个苹果很好吃。", "This apple is very tasty."),
    ("你的名字很好。", "Your name is nice."),
    ("我的朋友很好。", "My friend is very nice."),
    ("这些苹果太少了。", "These apples are too few."),
    ("学校的老师都很好。", "The teachers at the school are all very good."),
]

# --- School and study ---
HSK1 += [
    ("我是这个学校的学生。", "I am a student at this school."),
    ("我们的老师是中国人。", "Our teacher is Chinese."),
    ("我喜欢我们的学校。", "I like our school."),
    ("我每天学习汉语。", "I study Chinese every day."),
    ("学习汉语很有意思。", "Studying Chinese is very interesting."),
    ("我会写很多汉字。", "I can write many Chinese characters."),
    ("这个字怎么读？", "How do you read this character?"),
    ("请读这本书。", "Please read this book."),
    ("老师在教室里写字。", "The teacher is writing characters in the classroom."),
    ("我的同学都很喜欢学习。", "My classmates all like studying."),
    ("我们上午学习，下午回家。", "We study in the morning and go home in the afternoon."),
    ("这本书是老师的。", "This book is the teacher's."),
    ("我想学习写汉字。", "I want to learn to write Chinese characters."),
    ("你们今天学习什么？", "What are you studying today?"),
    ("我们今天学习汉语。", "We are studying Chinese today."),
    ("学校里有很多书。", "There are many books at the school."),
    ("老师，这个字怎么写？", "Teacher, how do you write this character?"),
    ("我看不懂这本书。", "I can't understand this book."),
    ("我会说也会写。", "I can speak and also write."),
    ("我的汉语老师很好。", "My Chinese teacher is very good."),
]

# --- Shopping and money ---
HSK1 += [
    ("我去商店买衣服。", "I'm going to the store to buy clothes."),
    ("这件衣服多少钱？", "How much is this piece of clothing?"),
    ("这件衣服一百块钱。", "This piece of clothing is one hundred yuan."),
    ("太贵了，我不买。", "It's too expensive, I won't buy it."),
    ("我想买这个杯子。", "I want to buy this cup."),
    ("你想买什么东西？", "What do you want to buy?"),
    ("我买了一些水果和茶。", "I bought some fruit and tea."),
    ("商店里的东西很多。", "There are many things in the store."),
    ("这本书不贵。", "This book is not expensive."),
    ("我没有钱买这个。", "I don't have money to buy this."),
    ("请问，这个多少钱？", "Excuse me, how much is this?"),
    ("我想买一个电脑。", "I want to buy a computer."),
    ("这个商店的水果很好。", "The fruit in this store is very good."),
    ("我买东西的时候没有钱。", "I had no money when I was shopping."),
    ("你买了什么？", "What did you buy?"),
]

# --- Transport and travel ---
HSK1 += [
    ("我坐出租车去医院。", "I take a taxi to the hospital."),
    ("我们坐飞机去北京。", "We take a plane to Beijing."),
    ("飞机现在几点？", "What time is the plane now?"),
    ("我想坐飞机去中国。", "I want to take a plane to China."),
    ("出租车在前面。", "The taxi is up ahead."),
    ("我每天坐车去学校。", "I take a car to school every day."),
    ("我们坐这个车回家吧。", "Let's take this car home."),
    ("他坐飞机回家了。", "He took a plane home."),
    ("去医院怎么走？", "How do you get to the hospital?"),
    ("我想去北京看朋友。", "I want to go to Beijing to see a friend."),
]

# --- Health and the body ---
HSK1 += [
    ("我想睡觉。", "I want to sleep."),
    ("我每天晚上十点睡觉。", "I go to sleep at ten every night."),
    ("我去医院看医生。", "I'm going to the hospital to see a doctor."),
    ("医生说我没关系。", "The doctor says I'm fine."),
    ("他在医院里。", "He is in the hospital."),
    ("我想喝点儿热水。", "I want to drink some hot water."),
    ("我有点儿累，想睡觉。", "I'm a little tired and want to sleep."),
    ("妈妈在医院工作。", "Mom works at the hospital."),
    ("你想休息一下吗？", "Do you want to rest a little?"),
    ("我中午想睡一会儿。", "I want to sleep for a while at noon."),
]

# --- Mixed everyday sentences ---
HSK1 += [
    ("我们是好朋友。", "We are good friends."),
    ("他和我都喜欢看书。", "He and I both like reading."),
    ("我和妈妈一起去商店。", "Mom and I go to the store together."),
    ("请给我那本书。", "Please give me that book."),
    ("这是给你的。", "This is for you."),
    ("我有一些问题。", "I have some questions."),
    ("我不知道他的名字。", "I don't know his name."),
    ("你知道现在几点吗？", "Do you know what time it is now?"),
    ("我想问你一个问题。", "I want to ask you a question."),
    ("请问，你叫什么名字？", "Excuse me, what is your name?"),
    ("他是一个很好的人。", "He is a very good person."),
    ("我们都是学生。", "We are all students."),
    ("今天是个好天气。", "Today is good weather."),
    ("我想在家看书。", "I want to read at home."),
    ("他不在家。", "He is not at home."),
    ("妈妈，我回来了。", "Mom, I'm back."),
    ("你看，那是我的猫。", "Look, that is my cat."),
    ("这是我的电话。", "This is my phone."),
    ("我喜欢和朋友一起吃饭。", "I like eating with friends."),
    ("我的朋友住在北京。", "My friend lives in Beijing."),
]

# --- Conversations and suggestions ---
HSK1 += [
    ("喂，你好，请问你是谁？", "Hello, who is this please?"),
    ("我们一起去看电影，怎么样？", "Let's go see a movie together, how about it?"),
    ("好的，几点去？", "Okay, what time shall we go?"),
    ("我们下午三点去，好吗？", "Let's go at three in the afternoon, okay?"),
    ("我们去喝茶吧。", "Let's go drink tea."),
    ("你想喝点儿什么？", "What would you like to drink?"),
    ("我想喝茶，谢谢。", "I'd like some tea, thank you."),
    ("你今天来我家吗？", "Are you coming to my house today?"),
    ("我今天很忙，明天去你家。", "I'm busy today, I'll go to your house tomorrow."),
    ("你能不能给我打电话？", "Can you give me a call?"),
    ("我现在不能去，请等一下。", "I can't go now, please wait a moment."),
    ("你说什么？我没听见。", "What did you say? I didn't hear."),
    ("请你说慢一点儿。", "Please speak a little slower."),
    ("我听不懂，请你再说一次。", "I don't understand, please say it again."),
    ("没问题，我们明天见。", "No problem, see you tomorrow."),
    ("你今天怎么不去学校？", "Why aren't you going to school today?"),
    ("我有点儿不高兴。", "I'm a little unhappy."),
    ("不要去，太冷了。", "Don't go, it's too cold."),
    ("我们去那个新饭店，怎么样？", "Let's go to that new restaurant, how about it?"),
    ("好，我很喜欢那个饭店。", "Sure, I really like that restaurant."),
]

# --- Daily routine ---
HSK1 += [
    ("我每天上午去学校。", "I go to school every morning."),
    ("我中午在学校吃饭。", "I eat lunch at school."),
    ("下午我回家看书。", "In the afternoon I go home and read."),
    ("晚上我和家人一起吃饭。", "In the evening I eat with my family."),
    ("我每天看一点儿电视。", "I watch a little TV every day."),
    ("我喜欢早上喝茶。", "I like drinking tea in the morning."),
    ("他每天晚上学习汉语。", "He studies Chinese every evening."),
    ("我们上午有四个小时的课。", "We have four hours of class in the morning."),
    ("我每天都很高兴。", "I am happy every day."),
    ("妈妈每天做很好吃的菜。", "Mom makes very tasty food every day."),
    ("我们晚上九点回家。", "We go home at nine in the evening."),
    ("他下午去买东西。", "He goes shopping in the afternoon."),
    ("我现在去睡觉。", "I'm going to sleep now."),
    ("我每天读一本书。", "I read a book every day."),
    ("今天我没有课。", "I don't have class today."),
]

# --- Things and objects ---
HSK1 += [
    ("这是我的桌子。", "This is my desk."),
    ("那是谁的椅子？", "Whose chair is that?"),
    ("我的衣服在椅子上。", "My clothes are on the chair."),
    ("桌子上有一个杯子。", "There is a cup on the table."),
    ("我想买一个新电脑。", "I want to buy a new computer."),
    ("这个电脑是我爸爸的。", "This computer is my father's."),
    ("我的电话在桌子上。", "My phone is on the table."),
    ("这本书是新的。", "This book is new."),
    ("我有很多衣服。", "I have a lot of clothes."),
    ("这些是我的东西。", "These are my things."),
    ("那个杯子里有水。", "There is water in that cup."),
    ("我的书和电脑都在家里。", "My books and computer are both at home."),
    ("请看桌子上的书。", "Please look at the book on the table."),
    ("这是什么字？", "What character is this?"),
    ("我不认识这个字。", "I don't recognize this character."),
]

# --- More questions and answers ---
HSK1 += [
    ("你叫我吗？", "Are you calling me?"),
    ("是的，我叫你。", "Yes, I'm calling you."),
    ("你看见我的书了吗？", "Did you see my book?"),
    ("我没看见你的书。", "I didn't see your book."),
    ("你今天有课吗？", "Do you have class today?"),
    ("我今天上午有课。", "I have class this morning."),
    ("你想喝茶还是喝水？", "Do you want tea or water?"),
    ("我想喝水，谢谢。", "I want water, thank you."),
    ("你的家在哪儿？", "Where is your home?"),
    ("我的家在学校前面。", "My home is in front of the school."),
    ("这是不是你的猫？", "Is this your cat or not?"),
    ("这不是我的猫。", "This is not my cat."),
    ("你认识不认识他？", "Do you know him or not?"),
    ("我认识他，他是我的同学。", "I know him, he is my classmate."),
    ("你去不去商店？", "Are you going to the store or not?"),
    ("我去，我们一起去吧。", "I'm going, let's go together."),
    ("你吃了吗？", "Have you eaten?"),
    ("我还没吃。", "I haven't eaten yet."),
    ("他来了吗？", "Has he come?"),
    ("他还没来。", "He hasn't come yet."),
]

# --- People and relationships ---
HSK1 += [
    ("他是我的好朋友。", "He is my good friend."),
    ("我们是同学，也是朋友。", "We are classmates and also friends."),
    ("我妈妈的朋友是医生。", "My mom's friend is a doctor."),
    ("我爸爸有很多朋友。", "My father has many friends."),
    ("那个先生是我的老师。", "That gentleman is my teacher."),
    ("那个小姐很漂亮。", "That young lady is very pretty."),
    ("他的女儿是学生。", "His daughter is a student."),
    ("我儿子喜欢小狗。", "My son likes little dogs."),
    ("我认识她的爸爸妈妈。", "I know her mother and father."),
    ("我们都很喜欢这个老师。", "We all like this teacher very much."),
    ("他是一个很高兴的人。", "He is a very cheerful person."),
    ("我的同学有很多书。", "My classmate has many books."),
    ("你的朋友叫什么名字？", "What is your friend's name?"),
    ("我朋友的名字叫王月。", "My friend's name is Wang Yue."),
    ("她和她的女儿都很漂亮。", "She and her daughter are both very pretty."),
]

# --- Expressing ability and possibility ---
HSK1 += [
    ("我会写汉字。", "I can write Chinese characters."),
    ("他会做菜。", "He can cook."),
    ("我能看懂这本书。", "I can understand this book."),
    ("你能帮我吗？", "Can you help me?"),
    ("我现在不能去。", "I can't go now."),
    ("明天我能来。", "I can come tomorrow."),
    ("你会不会说汉语？", "Can you speak Chinese?"),
    ("我会一点儿。", "I can a little."),
    ("这个我不会做。", "I can't do this."),
    ("我能喝一点儿茶吗？", "Can I have a little tea?"),
]

# --- Time duration and frequency ---
HSK1 += [
    ("我学习汉语两年了。", "I have been studying Chinese for two years."),
    ("我在北京住了三个月。", "I lived in Beijing for three months."),
    ("他每天看书两个小时。", "He reads for two hours every day."),
    ("我们等了十分钟。", "We waited for ten minutes."),
    ("我每个星期去两次商店。", "I go to the store twice a week."),
    ("他来中国一年了。", "He has been in China for a year."),
    ("我每天睡八个小时。", "I sleep for eight hours every day."),
    ("请等我几分钟。", "Please wait a few minutes for me."),
    ("我们说了很长时间。", "We talked for a long time."),
    ("我学了三个月汉语。", "I studied Chinese for three months."),
]

# --- Feelings and reactions ---
HSK1 += [
    ("见到你我很高兴。", "I'm very happy to see you."),
    ("今天是我的生日，我很高兴。", "Today is my birthday, I'm very happy."),
    ("这个电影很好，我很喜欢。", "This movie is great, I like it a lot."),
    ("天气好的时候我很高兴。", "I'm happy when the weather is good."),
    ("他不在，我有点儿不高兴。", "He's not here, I'm a little unhappy."),
    ("谢谢你的茶，很好喝。", "Thank you for the tea, it's very good."),
    ("这本书太有意思了。", "This book is so interesting."),
    ("我很喜欢这里。", "I really like it here."),
    ("能认识你，我很高兴。", "I'm very glad to meet you."),
    ("今天的菜很好吃，谢谢妈妈。", "Today's food is delicious, thank you Mom."),
]

# --- Polite and useful phrases ---
HSK1 += [
    ("请问，医院在哪儿？", "Excuse me, where is the hospital?"),
    ("请问，现在几点了？", "Excuse me, what time is it now?"),
    ("对不起，我来晚了。", "Sorry, I'm late."),
    ("没关系，请坐。", "It's okay, please sit."),
    ("谢谢你帮我。", "Thank you for helping me."),
    ("请慢慢吃。", "Please eat slowly (enjoy your meal)."),
    ("请等一下，我现在来。", "Please wait a moment, I'm coming now."),
    ("请问，你是李老师吗？", "Excuse me, are you Teacher Li?"),
    ("不好意思，我没有钱。", "Sorry, I don't have money."),
    ("请你给我看看。", "Please let me have a look."),
]

# --- Wrap-up everyday sentences ---
HSK1 += [
    ("我喜欢中国和中国菜。", "I like China and Chinese food."),
    ("今天我们学了很多字。", "We learned many characters today."),
    ("我想和你做朋友。", "I want to be your friend."),
    ("他的汉语说得很好。", "He speaks Chinese very well."),
    ("我们一家人都在北京。", "Our whole family is in Beijing."),
    ("我喜欢看中国电影。", "I like watching Chinese movies."),
    ("明天我不去学校，我去看朋友。", "I'm not going to school tomorrow, I'm visiting a friend."),
    ("我家的小猫很喜欢喝水。", "My family's cat loves drinking water."),
    ("这是我第一次来中国。", "This is my first time coming to China."),
    ("我们晚上一起看电视吧。", "Let's watch TV together this evening."),
    ("我想给妈妈买一些水果。", "I want to buy some fruit for Mom."),
    ("你的电脑在桌子上，请看。", "Your computer is on the table, please look."),
    ("我每天都想学习新的汉字。", "I want to learn new characters every day."),
    ("我们的老师今天不在学校。", "Our teacher is not at school today."),
    ("他喜欢喝茶，我喜欢喝水。", "He likes tea, I like water."),
    ("今天天气很好，我们去走走吧。", "The weather is nice today, let's go for a walk."),
    ("我的朋友明天来我家吃饭。", "My friend is coming to my house to eat tomorrow."),
    ("请问，这是去北京的飞机吗？", "Excuse me, is this the plane to Beijing?"),
    ("我没有时间，明天再说吧。", "I don't have time, let's talk tomorrow."),
    ("我们都很喜欢学习汉语。", "We all really like studying Chinese."),
]

# --- Final HSK1 batch ---
HSK1 += [
    ("你的猫叫什么名字？", "What is your cat's name?"),
    ("我的猫没有名字。", "My cat doesn't have a name."),
    ("我想喝一点儿热茶。", "I'd like a little hot tea."),
    ("这个苹果是给你的。", "This apple is for you."),
    ("我们坐在椅子上看书。", "We sit on the chairs and read."),
    ("老师写的字很漂亮。", "The characters the teacher writes are very pretty."),
    ("我爸爸明天去北京工作。", "My dad is going to Beijing for work tomorrow."),
    ("你想不想看这个电影？", "Do you want to watch this movie?"),
    ("我现在很高兴，因为今天天气很好。", "I'm happy now because the weather is nice today."),
    ("这个商店的衣服很漂亮。", "The clothes in this store are very pretty."),
    ("我每天上午都喝一杯茶。", "I drink a cup of tea every morning."),
    ("我们学校有很多中国老师。", "Our school has many Chinese teachers."),
    ("他不喜欢冷天，喜欢热天。", "He doesn't like cold days, he likes hot days."),
    ("请你写一下你的名字。", "Please write your name."),
    ("我想买这本书，多少钱？", "I want to buy this book, how much is it?"),
    ("你家有几个猫？", "How many cats does your family have?"),
    ("我们一起去医院看朋友吧。", "Let's go to the hospital to visit a friend."),
    ("这个字我会写，那个字我不会。", "I can write this character, but not that one."),
    ("妈妈在饭店里等我们。", "Mom is waiting for us in the restaurant."),
    ("我想在中国学习汉语。", "I want to study Chinese in China."),
    ("今天的水果很好吃，谢谢你。", "Today's fruit is delicious, thank you."),
    ("你能给我打个电话吗？", "Can you give me a call?"),
    ("我没有时间看电视。", "I don't have time to watch TV."),
    ("他坐出租车去机场。", "He takes a taxi to the airport."),
    ("我们家后面有一个小商店。", "There is a little store behind our house."),
    ("这些衣服都是我女儿的。", "These clothes are all my daughter's."),
    ("现在是下午两点。", "It is two in the afternoon now."),
    ("我喜欢和爸爸妈妈一起去商店。", "I like going to the store with my parents."),
    ("请你们都坐下。", "Please everyone sit down."),
    ("我很喜欢这里，这里的人很好。", "I really like it here, the people here are nice."),
]

# ===================== HSK 2 — 500 sentences =====================

# --- Comparisons (比, 最) ---
HSK2 += [
    ("我比你高。", "I am taller than you."),
    ("他比我忙。", "He is busier than me."),
    ("今天比昨天冷。", "Today is colder than yesterday."),
    ("这个房间比那个房间大。", "This room is bigger than that room."),
    ("坐飞机比坐车快。", "Taking a plane is faster than taking a car."),
    ("这件衣服比那件便宜。", "This piece of clothing is cheaper than that one."),
    ("我哥哥比我大三岁。", "My older brother is three years older than me."),
    ("这本书比那本贵一点儿。", "This book is a little more expensive than that one."),
    ("他跑得比我快。", "He runs faster than me."),
    ("今天的题比昨天的多。", "There are more questions today than yesterday."),
    ("我觉得咖啡比茶好喝。", "I think coffee tastes better than tea."),
    ("我最喜欢的运动是游泳。", "My favorite sport is swimming."),
    ("这是我最好的朋友。", "This is my best friend."),
    ("这家饭店的菜最好吃。", "This restaurant's food is the tastiest."),
    ("我们班他学习最好。", "In our class he studies the best."),
    ("这条路最近。", "This road is the closest."),
    ("我最喜欢红色。", "I like red the most."),
    ("晴天是我最喜欢的天气。", "Sunny days are my favorite weather."),
    ("他是我们公司最忙的人。", "He is the busiest person in our company."),
    ("我妹妹比我跑得快。", "My younger sister runs faster than me."),
]

# --- Because / although ---
HSK2 += [
    ("因为下雨，所以我没去。", "Because it rained, I didn't go."),
    ("因为我生病了，所以没上班。", "Because I was sick, I didn't go to work."),
    ("因为今天很忙，所以我很累。", "Because I was busy today, I'm very tired."),
    ("我喜欢他，因为他很好。", "I like him because he is very nice."),
    ("虽然很累，但是我很高兴。", "Although I'm tired, I'm happy."),
    ("虽然他很忙，但是常常帮助我。", "Although he is busy, he often helps me."),
    ("虽然这个房间小，但是很漂亮。", "Although this room is small, it is pretty."),
    ("虽然天气不好，但是我们还是去了。", "Although the weather was bad, we still went."),
    ("因为路很远，所以我们坐车去。", "Because the road is far, we go by car."),
    ("因为他没准备，所以考试没考好。", "Because he didn't prepare, he didn't do well on the exam."),
    ("我没买，因为太贵了。", "I didn't buy it because it was too expensive."),
    ("因为我很喜欢中国，所以来这里学习。", "Because I love China, I came here to study."),
    ("虽然汉语课很长，但是很有意思。", "Although Chinese class is long, it is interesting."),
    ("因为已经很晚了，所以我要回家了。", "Because it's already late, I need to go home."),
    ("虽然我很想去，但是没有时间。", "Although I really want to go, I don't have time."),
]

# --- Aspect: 了, 过, 正在, 着 ---
HSK2 += [
    ("我已经吃过饭了。", "I have already eaten."),
    ("他去过中国两次。", "He has been to China twice."),
    ("我没去过北京。", "I have never been to Beijing."),
    ("你吃过中国菜吗？", "Have you ever eaten Chinese food?"),
    ("我正在做饭呢。", "I am cooking right now."),
    ("他正在打篮球。", "He is playing basketball right now."),
    ("妈妈正在洗衣服。", "Mom is washing clothes right now."),
    ("我们正在上课，请等一下。", "We are in class, please wait a moment."),
    ("门开着，请进。", "The door is open, please come in."),
    ("他笑着对我说话。", "He spoke to me with a smile."),
    ("外面下着雪。", "It is snowing outside."),
    ("我昨天看完了那本书。", "I finished reading that book yesterday."),
    ("我已经写完了。", "I have already finished writing."),
    ("他先走了。", "He left first."),
    ("电影已经开始了。", "The movie has already started."),
    ("我们已经准备好了。", "We are already prepared."),
    ("孩子们正在玩。", "The children are playing."),
    ("他穿着一件红色的衣服。", "He is wearing a red piece of clothing."),
    ("我已经知道这件事了。", "I already know about this matter."),
    ("你看过这个电影吗？", "Have you seen this movie?"),
]

# --- Sports and hobbies ---
HSK2 += [
    ("我喜欢游泳。", "I like swimming."),
    ("他每天早上跑步。", "He runs every morning."),
    ("我们一起去踢足球吧。", "Let's go play soccer together."),
    ("他打篮球打得很好。", "He plays basketball very well."),
    ("我妹妹喜欢跳舞。", "My younger sister likes dancing."),
    ("她唱歌唱得很好听。", "She sings very beautifully."),
    ("运动对身体很好。", "Exercise is good for the body."),
    ("我们星期六常常去运动。", "We often exercise on Saturdays."),
    ("你会游泳吗？", "Can you swim?"),
    ("我不太会跳舞。", "I'm not very good at dancing."),
    ("他喜欢早上去跑步。", "He likes to go running in the morning."),
    ("我们下午去打篮球，好吗？", "Let's go play basketball in the afternoon, okay?"),
    ("孩子们喜欢在外面玩。", "The children like to play outside."),
    ("跑步以后我觉得很累。", "After running I feel very tired."),
    ("他唱歌的时候很高兴。", "He is very happy when he sings."),
]

# --- Giving, helping, letting ---
HSK2 += [
    ("请帮我一下。", "Please help me a little."),
    ("谢谢你帮助我。", "Thank you for helping me."),
    ("他常常帮助同学。", "He often helps his classmates."),
    ("我给你介绍一下我的朋友。", "Let me introduce my friend to you."),
    ("妈妈给我做了好吃的菜。", "Mom made me delicious food."),
    ("这本书是我送给你的。", "This book is a gift from me to you."),
    ("他送我回家。", "He saw me home."),
    ("老师让我们准备考试。", "The teacher told us to prepare for the exam."),
    ("妈妈不让孩子看太多电视。", "Mom doesn't let the children watch too much TV."),
    ("你能帮我找一下我的手机吗？", "Can you help me find my phone?"),
    ("请给我一杯咖啡。", "Please give me a cup of coffee."),
    ("我想给妈妈打个电话。", "I want to call my mom."),
    ("他给了我一本书。", "He gave me a book."),
    ("让我看看你的新手表。", "Let me see your new watch."),
    ("我来介绍一下，这是我哥哥。", "Let me introduce: this is my older brother."),
]

# --- Opinions, knowing, hoping ---
HSK2 += [
    ("我觉得这个电影很好看。", "I think this movie is really good."),
    ("你觉得怎么样？", "What do you think?"),
    ("我觉得汉语很有意思。", "I think Chinese is very interesting."),
    ("我希望明天天气好。", "I hope the weather is good tomorrow."),
    ("我希望你快点儿好起来。", "I hope you get better soon."),
    ("我不知道他在哪儿。", "I don't know where he is."),
    ("你知道这个字的意思吗？", "Do you know the meaning of this character?"),
    ("我知道你很忙。", "I know you are very busy."),
    ("这个字是什么意思？", "What does this character mean?"),
    ("他可能不来了。", "He might not come."),
    ("明天可能会下雪。", "It might snow tomorrow."),
    ("我觉得这件衣服太贵了。", "I think this piece of clothing is too expensive."),
    ("我希望能去中国旅游。", "I hope I can travel to China."),
    ("你知道现在几点了吗？", "Do you know what time it is now?"),
    ("我真的不知道这个问题怎么回答。", "I really don't know how to answer this question."),
]

# --- Colors and appearance ---
HSK2 += [
    ("我喜欢红色。", "I like the color red."),
    ("这件衣服是白色的。", "This piece of clothing is white."),
    ("他有一个黑色的猫。", "He has a black cat."),
    ("你喜欢什么颜色？", "What color do you like?"),
    ("天是白的，雪也是白的。", "The sky is white, and the snow is white too."),
    ("她的眼睛很大很漂亮。", "Her eyes are big and pretty."),
    ("我想买一件红色的衣服。", "I want to buy a red piece of clothing."),
    ("这个房间的颜色很漂亮。", "The color of this room is very pretty."),
    ("他穿着一件黑色的衣服。", "He is wearing a black piece of clothing."),
    ("这些衣服有很多颜色。", "These clothes come in many colors."),
]

# --- Body and health ---
HSK2 += [
    ("我今天生病了。", "I am sick today."),
    ("你身体怎么样？", "How is your health?"),
    ("多运动对身体好。", "Exercising a lot is good for your health."),
    ("我今天觉得很累。", "I feel very tired today."),
    ("你要多喝热水，少吃药。", "You should drink more hot water and take less medicine."),
    ("医生让我多休息。", "The doctor told me to rest more."),
    ("他生病了，所以没来上课。", "He was sick, so he didn't come to class."),
    ("我的眼睛有点儿累。", "My eyes are a little tired."),
    ("吃了药以后，我好多了。", "After taking the medicine, I feel much better."),
    ("你别太累了，早点儿休息。", "Don't get too tired, rest early."),
    ("他身体很好，很少生病。", "He is very healthy and rarely gets sick."),
    ("我希望你的身体快点儿好。", "I hope you get well soon."),
    ("生病的时候要多喝水。", "When you're sick, you should drink lots of water."),
    ("妈妈身体不太好。", "Mom's health isn't very good."),
    ("天冷了，别生病。", "It's getting cold, don't get sick."),
]

# --- Travel and getting around ---
HSK2 += [
    ("我想去中国旅游。", "I want to travel to China."),
    ("我们坐公共汽车去学校。", "We take the bus to school."),
    ("机场离这儿很远。", "The airport is far from here."),
    ("我家离公司很近。", "My home is close to the company."),
    ("从这儿到机场要多长时间？", "How long does it take to get from here to the airport?"),
    ("请问，去机场怎么走？", "Excuse me, how do I get to the airport?"),
    ("我们住在一个很好的宾馆。", "We are staying at a very nice hotel."),
    ("这条路很长。", "This road is very long."),
    ("往前走，宾馆就在右边。", "Go straight ahead, the hotel is on the right."),
    ("学校在医院的旁边。", "The school is next to the hospital."),
    ("出租车比公共汽车快。", "A taxi is faster than the bus."),
    ("我们坐飞机去，因为飞机很快。", "We fly because the plane is fast."),
    ("从我家到学校很近。", "It's close from my home to school."),
    ("我们旅游的时候很高兴。", "We were very happy when we traveled."),
    ("请往左边走。", "Please go to the left."),
    ("医院在商店的右边。", "The hospital is to the right of the store."),
    ("我第一次坐飞机，很高兴。", "It was my first time on a plane, I was very excited."),
    ("我们去旅游，住了三天宾馆。", "We went traveling and stayed at a hotel for three days."),
    ("机场里有很多人。", "There are many people at the airport."),
    ("到了北京，我给你打电话。", "When I arrive in Beijing, I'll call you."),
]

# --- Work and school ---
HSK2 += [
    ("我每天八点上班。", "I go to work at eight every day."),
    ("他在一家大公司工作。", "He works at a big company."),
    ("我们明天有汉语考试。", "We have a Chinese exam tomorrow."),
    ("这次考试很长。", "This exam is very long."),
    ("我要准备明天的课。", "I need to prepare for tomorrow's class."),
    ("这个问题我不会回答。", "I don't know how to answer this question."),
    ("老师问了我一个问题。", "The teacher asked me a question."),
    ("请告诉我怎么做。", "Please tell me how to do it."),
    ("这道题我不会做。", "I don't know how to do this problem."),
    ("我们的教室在学校里。", "Our classroom is in the school."),
    ("他上班的时候很忙。", "He is very busy at work."),
    ("考试以前我准备了很多。", "I prepared a lot before the exam."),
    ("老师让我们回答问题。", "The teacher had us answer questions."),
    ("我已经做完今天的题了。", "I have already finished today's problems."),
    ("你今天上班吗？", "Are you going to work today?"),
    ("公司离我家不远。", "The company isn't far from my home."),
    ("我喜欢我的工作。", "I like my job."),
    ("我们开始上课吧。", "Let's begin class."),
    ("他考试考得很好。", "He did very well on the exam."),
    ("这个问题很有意思。", "This question is very interesting."),
]

# --- Time words: already, just, still, from-to ---
HSK2 += [
    ("我已经吃饭了。", "I have already eaten."),
    ("他已经走了。", "He has already left."),
    ("现在还早，再睡一会儿吧。", "It's still early, sleep a little longer."),
    ("我还没准备好。", "I'm not ready yet."),
    ("你还想喝点儿什么吗？", "Would you still like something to drink?"),
    ("吃完饭就去吧。", "Let's go right after eating."),
    ("我一回家就睡觉了。", "I went to sleep as soon as I got home."),
    ("从早上到现在我都很忙。", "I've been busy from morning until now."),
    ("我们再等他五分钟。", "Let's wait five more minutes for him."),
    ("请再说一次。", "Please say it again."),
    ("他每天都很早起床。", "He gets up very early every day."),
    ("我去年去过中国。", "I went to China last year."),
    ("已经九点了，我们快走吧。", "It's already nine o'clock, let's hurry."),
    ("从这儿到那儿不太远。", "It's not too far from here to there."),
    ("他从早上开始就在工作。", "He has been working since this morning."),
]

# --- Food and restaurant (expanded) ---
HSK2 += [
    ("服务员，我们想点菜。", "Waiter, we'd like to order."),
    ("我想吃面条。", "I want to eat noodles."),
    ("早上我喝牛奶，吃鸡蛋。", "In the morning I drink milk and eat eggs."),
    ("这家饭店的鱼很好吃。", "The fish at this restaurant is delicious."),
    ("我不太喜欢吃羊肉。", "I don't really like eating mutton."),
    ("天热的时候我喜欢吃西瓜。", "When it's hot I like eating watermelon."),
    ("来一杯咖啡，谢谢。", "A cup of coffee, please."),
    ("这个面条太长了。", "These noodles are too long."),
    ("服务员，这个菜不太好吃。", "Waiter, this dish isn't very tasty."),
    ("我们点了很多菜。", "We ordered a lot of dishes."),
    ("你想喝牛奶还是咖啡？", "Do you want milk or coffee?"),
    ("这家店的鸡蛋面很好吃。", "The egg noodles at this place are delicious."),
    ("这个西瓜很大，也很便宜。", "This watermelon is big and cheap."),
    ("我每天早上都喝一杯牛奶。", "I drink a glass of milk every morning."),
    ("这条鱼真大！", "This fish is really big!"),
    ("我觉得羊肉比鸡肉好吃。", "I think mutton is tastier than chicken."),
    ("吃完饭，我们去喝咖啡吧。", "After eating, let's go for coffee."),
    ("这个饭店的服务员很好。", "The waiters at this restaurant are very nice."),
    ("我想喝点儿热牛奶。", "I'd like some hot milk."),
    ("面条好了，快来吃吧。", "The noodles are ready, come eat."),
]

# --- Shopping and money (expanded) ---
HSK2 += [
    ("这件衣服太贵了，便宜一点儿吧。", "This is too expensive, make it cheaper."),
    ("这个手机多少钱？", "How much is this phone?"),
    ("这个手机一千块钱。", "This phone is one thousand yuan."),
    ("那本书很便宜，五块钱。", "That book is cheap, five yuan."),
    ("我想买一块手表。", "I want to buy a watch."),
    ("这件红色的衣服我很喜欢，我买了。", "I really like this red one, I'll buy it."),
    ("商店里的东西很多，也很便宜。", "The store has many things, and they're cheap."),
    ("这个太贵，那个太便宜。", "This one is too expensive, that one is too cheap."),
    ("你这件衣服很漂亮，多少钱买的？", "Your clothing is pretty, how much did you pay?"),
    ("他卖水果，我常常去他那儿买。", "He sells fruit, and I often buy from him."),
    ("这些苹果怎么卖？", "How are these apples sold?"),
    ("两块钱一个。", "Two yuan each."),
    ("我买了一件新衣服。", "I bought a new piece of clothing."),
    ("这个手表有点儿贵。", "This watch is a little expensive."),
    ("便宜的东西也很好。", "Cheap things can also be good."),
]

# --- Requests, permission, plans (可以, 要, 想) ---
HSK2 += [
    ("我可以坐这儿吗？", "May I sit here?"),
    ("你可以帮我开门吗？", "Can you help me open the door?"),
    ("这儿可以打电话吗？", "Can I make a phone call here?"),
    ("我要去机场，请快一点儿。", "I need to go to the airport, please hurry."),
    ("明天我要早起。", "I have to get up early tomorrow."),
    ("我们要不要一起去？", "Shall we go together?"),
    ("你要喝点儿什么？", "What would you like to drink?"),
    ("我想休息一下，可以吗？", "I'd like to rest a bit, is that okay?"),
    ("我们可以再等一会儿。", "We can wait a little longer."),
    ("你可以告诉我为什么吗？", "Can you tell me why?"),
    ("我要回家了，再见。", "I have to go home now, goodbye."),
    ("别走，再坐一会儿吧。", "Don't go, sit a little longer."),
    ("我们什么时候可以开始？", "When can we start?"),
    ("你想什么时候去都可以。", "You can go whenever you want."),
    ("请问，可以进来吗？", "Excuse me, may I come in?"),
]

# --- Family (expanded with HSK2 kin terms) ---
HSK2 += [
    ("我有一个哥哥和一个妹妹。", "I have an older brother and a younger sister."),
    ("我姐姐比我大两岁。", "My older sister is two years older than me."),
    ("我弟弟很喜欢踢足球。", "My younger brother loves playing soccer."),
    ("这是我丈夫，他是医生。", "This is my husband, he is a doctor."),
    ("我妻子在一家公司上班。", "My wife works at a company."),
    ("我们一家人都很快乐。", "Our whole family is very happy."),
    ("孩子们都在房间里玩。", "The children are all playing in the room."),
    ("我哥哥的孩子已经五岁了。", "My older brother's child is already five years old."),
    ("妹妹的眼睛很大，很漂亮。", "My younger sister's eyes are big and pretty."),
    ("大家一起吃饭，真高兴。", "Everyone eating together, it's so nice."),
    ("我姐姐唱歌唱得很好。", "My older sister sings very well."),
    ("爸爸下班以后常常和我玩。", "Dad often plays with me after work."),
    ("我和哥哥一起去打篮球。", "My older brother and I go play basketball together."),
    ("妈妈让弟弟早点儿睡觉。", "Mom tells my younger brother to sleep early."),
    ("生日快乐！", "Happy birthday!"),
]

# --- Weather (expanded) ---
HSK2 += [
    ("今天是晴天，天气很好。", "Today is sunny, the weather is nice."),
    ("外面下雪了，真漂亮。", "It's snowing outside, it's beautiful."),
    ("今天是阴天，可能会下雨。", "It's cloudy today, it might rain."),
    ("北京的天气很冷。", "Beijing's weather is very cold."),
    ("今天比昨天热多了。", "Today is much hotter than yesterday."),
    ("下雪的时候，孩子们很高兴。", "When it snows, the children are happy."),
    ("天气好的时候，我喜欢去运动。", "When the weather is nice, I like to exercise."),
    ("明天是晴天还是阴天？", "Will it be sunny or cloudy tomorrow?"),
    ("快下雨了，我们快回家吧。", "It's about to rain, let's hurry home."),
    ("今天天气真好，我们去走走吧。", "The weather is really nice today, let's go for a walk."),
    ("天冷的时候这儿常常下雪。", "When it's cold it often snows here."),
    ("今天有点儿阴，但是不冷。", "It's a bit cloudy today, but not cold."),
    ("下雪以后，外面很白。", "After it snows, everything outside is white."),
    ("今天的天气真热。", "Today's weather is really hot."),
    ("今天天气不错，我们去旅游吧。", "The weather is good today, let's go traveling."),
]

# --- Daily conversation (expanded) ---
HSK2 += [
    ("你好，请问你找谁？", "Hello, who are you looking for?"),
    ("喂，你现在忙不忙？", "Hello, are you busy now?"),
    ("对不起，我来晚了，路上人太多了。", "Sorry I'm late, there were too many people on the road."),
    ("没关系，我也是现在到。", "It's okay, I just got here too."),
    ("好长时间不见，你最近怎么样？", "Long time no see, how have you been?"),
    ("我最近很忙，但是很快乐。", "I've been busy lately, but happy."),
    ("欢迎你来我家。", "Welcome to my home."),
    ("请进，快坐。", "Please come in and have a seat."),
    ("你慢点儿走，路上人很多。", "Walk slowly, there are many people on the road."),
    ("谢谢你今天帮了我这么多。", "Thank you for helping me so much today."),
    ("你的汉语说得真好。", "You speak Chinese really well."),
    ("哪里哪里，我还要多学习。", "Not at all, I still need to study a lot."),
    ("我们很长时间没见了，一起吃个饭吧。", "We haven't seen each other in a long time, let's have a meal."),
    ("你先走吧，我还有点儿事。", "You go ahead, I still have something to do."),
    ("时间不早了，我得走了。", "It's getting late, I have to go."),
]

# --- Adjectives and states (expanded) ---
HSK2 += [
    ("今天我很累。", "I am very tired today."),
    ("他工作很忙。", "He is very busy with work."),
    ("这个孩子真快乐。", "This child is really happy."),
    ("这里的路很长，也很远。", "The road here is long and far."),
    ("他走得很快。", "He walks very fast."),
    ("请你说得慢一点儿。", "Please speak a little slower."),
    ("这个房间很大，也很漂亮。", "This room is big and pretty."),
    ("他个子很高。", "He is very tall."),
    ("这件衣服很新。", "This piece of clothing is very new."),
    ("这个手机不错，也不贵。", "This phone is good and not expensive."),
    ("今天我很高兴，因为是我的生日。", "I'm very happy today because it's my birthday."),
    ("他写的字很快，也很好。", "He writes characters quickly and well."),
    ("这家饭店离我家很近。", "This restaurant is close to my home."),
    ("学校离这儿不远，走路就到。", "The school isn't far, you can walk there."),
    ("我觉得有点儿累，想休息一下。", "I feel a little tired and want to rest."),
    ("他真是一个快乐的人。", "He really is a happy person."),
    ("今天的题不太多。", "There aren't too many questions today."),
    ("这个东西太贵了，我不想买。", "This thing is too expensive, I don't want to buy it."),
    ("他的房间很新，也很漂亮。", "His room is new and pretty."),
    ("现在天气好了。", "The weather is nice now."),
]

# --- More verbs: wear, wash, find, get up, rest ---
HSK2 += [
    ("今天很冷，多穿点儿衣服。", "It's cold today, wear more clothes."),
    ("我要先洗手再吃饭。", "I'll wash my hands before eating."),
    ("我找不到我的手机了。", "I can't find my phone."),
    ("你找到工作了吗？", "Have you found a job?"),
    ("我每天七点起床。", "I get up at seven every day."),
    ("起床以后，我先喝一杯水。", "After getting up, I drink a glass of water first."),
    ("你穿这件衣服很漂亮。", "You look pretty in this piece of clothing."),
    ("姐姐正在洗衣服。", "My older sister is washing clothes."),
    ("我们休息一下，再开始吧。", "Let's rest a bit and then start again."),
    ("他笑着走进了教室。", "He walked into the classroom smiling."),
    ("请等一下，我现在出来。", "Please wait a moment, I'm coming out now."),
    ("你听懂老师说的话了吗？", "Did you understand what the teacher said?"),
    ("这个问题我没听懂。", "I didn't understand this question."),
    ("天气冷了，要穿多点儿。", "It's getting cold, wear more."),
    ("他洗完手就吃饭了。", "He washed his hands and then ate."),
]

# --- Quantity and measure words ---
HSK2 += [
    ("我买了两件衣服。", "I bought two pieces of clothing."),
    ("请给我一件红色的。", "Please give me a red one."),
    ("这本书我看了三次了。", "I've read this book three times."),
    ("我去过中国很多次。", "I've been to China many times."),
    ("桌子上有两个鸡蛋。", "There are two eggs on the table."),
    ("这条鱼很大。", "This fish is very big."),
    ("我喝了两杯咖啡。", "I drank two cups of coffee."),
    ("他有很多件衣服。", "He has many pieces of clothing."),
    ("这是我第一次来这儿。", "This is my first time here."),
    ("他是我们班第一名。", "He is first in our class."),
    ("一年有十二个月。", "There are twelve months in a year."),
    ("一个星期有七天。", "There are seven days in a week."),
    ("请给我两个苹果。", "Please give me two apples."),
    ("这次旅游我们玩得很高兴。", "We had a great time on this trip."),
    ("我每天看两个小时的书。", "I read for two hours every day."),
]

# --- Telling, asking, answering ---
HSK2 += [
    ("请告诉我你的名字。", "Please tell me your name."),
    ("他告诉我他要去旅游。", "He told me he is going to travel."),
    ("我想问你一个问题。", "I want to ask you a question."),
    ("老师问的问题我都会回答。", "I can answer all the questions the teacher asks."),
    ("你为什么不告诉我？", "Why didn't you tell me?"),
    ("我不知道怎么回答这个问题。", "I don't know how to answer this question."),
    ("请回答我的问题。", "Please answer my question."),
    ("他没有回答我。", "He didn't answer me."),
    ("你能告诉我为什么吗？", "Can you tell me why?"),
    ("妈妈告诉我要好好学习。", "Mom told me to study hard."),
    ("我问他，他说他不知道。", "I asked him, and he said he doesn't know."),
    ("有问题就问老师。", "If you have a question, ask the teacher."),
    ("这个问题我回答不了。", "I can't answer this question."),
    ("请你告诉大家这件事。", "Please tell everyone about this."),
    ("老师告诉我们明天考试。", "The teacher told us there's an exam tomorrow."),
]

# --- Plans and future ---
HSK2 += [
    ("明年我想去中国学习。", "Next year I want to go to China to study."),
    ("这个星期六我们去旅游。", "This Saturday we're going on a trip."),
    ("下课以后你要做什么？", "What are you going to do after class?"),
    ("我准备明天去机场看朋友。", "I'm planning to go to the airport to see a friend tomorrow."),
    ("晚上我们一起去看电影吧。", "Let's go see a movie together tonight."),
    ("我想学好汉语，以后去中国工作。", "I want to learn Chinese well and work in China later."),
    ("等我有时间，我就去看你。", "When I have time, I'll come visit you."),
    ("我们想坐飞机去。", "We want to fly there."),
    ("吃完饭以后我要去运动。", "I'm going to exercise after eating."),
    ("明天上午我要去看医生。", "Tomorrow morning I'm going to see a doctor."),
    ("不上班的时候我想休息几天。", "When I'm not working I want to rest for a few days."),
    ("我想给妈妈买一件新衣服。", "I want to buy Mom a new piece of clothing."),
    ("我们下个月去北京旅游。", "We're going to Beijing next month."),
    ("学完这本书，我们就开始新的。", "After we finish this book, we'll start a new one."),
    ("我想以后做一名医生。", "I want to be a doctor in the future."),
]

# --- Likes, opinions, feelings (expanded) ---
HSK2 += [
    ("我非常喜欢这个学校。", "I really like this school."),
    ("这个电影真有意思。", "This movie is really interesting."),
    ("我觉得学习汉语很有意思。", "I think studying Chinese is very interesting."),
    ("他对我很好，我很高兴。", "He is very nice to me, and I'm happy."),
    ("这件事让我很高兴。", "This made me very happy."),
    ("听到这件事，大家都笑了。", "Hearing this, everyone laughed."),
    ("我最喜欢的运动是游泳和跑步。", "My favorite sports are swimming and running."),
    ("这本书很好看，我看了两次。", "This book is great, I've read it twice."),
    ("我真的很喜欢中国菜。", "I really like Chinese food."),
    ("天气这么好，我也很高兴。", "The weather is so nice, I'm happy too."),
    ("他说的话很有意思，大家都笑了。", "What he said was funny, and everyone laughed."),
    ("我非常希望能再来中国。", "I really hope I can come to China again."),
    ("这次旅游让我很快乐。", "This trip made me very happy."),
    ("他对工作非常认真。", "He is very serious about his work."),
    ("能帮助别人，我觉得很快乐。", "I feel happy being able to help others."),
]

# --- Location and directions (expanded) ---
HSK2 += [
    ("医院在饭店的左边。", "The hospital is to the left of the restaurant."),
    ("我的手机在桌子上边。", "My phone is on top of the table."),
    ("猫在椅子下边。", "The cat is under the chair."),
    ("学校的旁边有一个商店。", "There is a store next to the school."),
    ("请问，洗手间在哪儿？", "Excuse me, where is the restroom?"),
    ("往前走就到了。", "Go straight ahead and you'll be there."),
    ("从这儿往右边走。", "Go to the right from here."),
    ("他家离我家很近，走路五分钟。", "His home is close to mine, five minutes' walk."),
    ("我们公司在那个大门的右边。", "Our company is to the right of that big gate."),
    ("教室外边有很多学生。", "There are many students outside the classroom."),
    ("书店在医院和学校的中间。", "The bookstore is between the hospital and the school."),
    ("你坐在我的旁边吧。", "Sit next to me."),
    ("外边在下雨，别出去了。", "It's raining outside, don't go out."),
    ("请开车到门前面。", "Please drive the car to the front of the door."),
    ("我家就在学校后边。", "My home is right behind the school."),
]

# --- Studying Chinese (expanded) ---
HSK2 += [
    ("学习汉语要多说多听。", "To learn Chinese you should speak and listen a lot."),
    ("这个汉字我会写了。", "I can write this character now."),
    ("你懂这个字的意思吗？", "Do you understand this character's meaning?"),
    ("我每天学习两个小时的汉语。", "I study Chinese for two hours every day."),
    ("老师说我的汉语很好。", "The teacher says my Chinese is very good."),
    ("学习汉语虽然累，但是很有意思。", "Studying Chinese is tiring but interesting."),
    ("请问，这个字怎么读？", "Excuse me, how do you read this character?"),
    ("我看得懂，但是说得不太好。", "I can read it, but I don't speak it very well."),
    ("学了一年，我会说很多了。", "After a year of study, I can say a lot now."),
    ("我想多认识一些中国朋友。", "I want to get to know more Chinese friends."),
    ("他汉语说得非常好。", "He speaks Chinese very well."),
    ("多看中国电影对学习有帮助。", "Watching Chinese movies helps with learning."),
    ("我们今天学了二十个新字。", "We learned twenty new characters today."),
    ("这本书我看不懂。", "I can't understand this book."),
    ("我要好好准备明天的考试。", "I'm going to prepare well for tomorrow's exam."),
]

# --- Service and getting things done ---
HSK2 += [
    ("服务员，请给我们两杯水。", "Waiter, please give us two glasses of water."),
    ("您好，请问要点儿什么？", "Hello, what would you like to order?"),
    ("欢迎欢迎，里边请。", "Welcome, please come inside."),
    ("请问，这儿可以坐吗？", "Excuse me, can I sit here?"),
    ("我要一个鸡蛋面，谢谢。", "I'll have an egg noodle, thank you."),
    ("请问，洗手间往哪边走？", "Excuse me, which way is the restroom?"),
    ("一共多少钱？", "How much is it altogether?"),
    ("一共三十五块。", "Thirty-five yuan in total."),
    ("请等一下，我去给您找。", "Please wait a moment, I'll go get it for you."),
    ("您要的咖啡来了。", "Here is the coffee you ordered."),
    ("不好意思，让您等了。", "Sorry to keep you waiting."),
    ("请问，你们几点关门？", "Excuse me, what time do you close?"),
    ("这个可以便宜点儿吗？", "Can this be a bit cheaper?"),
    ("好的，给您便宜五块。", "Okay, I'll give you five yuan off."),
    ("谢谢，欢迎再来。", "Thank you, please come again."),
]

# --- Everyday moments ---
HSK2 += [
    ("早上好，今天天气真不错。", "Good morning, the weather is really nice today."),
    ("他一边吃饭一边看电视。", "He eats while watching TV."),
    ("孩子笑得很高兴。", "The child laughs happily."),
    ("我们坐在外边喝咖啡。", "We sit outside drinking coffee."),
    ("他打篮球打得很累。", "He played basketball until he was tired."),
    ("妈妈做的面条真好吃。", "The noodles Mom makes are really tasty."),
    ("天黑了，我们快回家吧。", "It's getting dark, let's hurry home."),
    ("他看着书睡着了。", "He fell asleep reading a book."),
    ("我们一起唱歌，一起跳舞。", "We sing and dance together."),
    ("下班以后，他常常去跑步。", "After work, he often goes running."),
    ("孩子们在外边玩得很高兴。", "The children are playing happily outside."),
    ("他每天早上都去运动。", "He goes exercising every morning."),
    ("我们在宾馆休息了一会儿。", "We rested for a while at the hotel."),
    ("今天真累，我想早点儿睡。", "I'm so tired today, I want to sleep early."),
    ("他笑了，因为他很高兴。", "He smiled because he was happy."),
]

# --- Mixed practical sentences ---
HSK2 += [
    ("我的手机找到了，在房间里。", "I found my phone, it was in the room."),
    ("他每天坐公共汽车上班。", "He takes the bus to work every day."),
    ("这件事我已经告诉他了。", "I have already told him about this."),
    ("我们公司今天很忙。", "Our company is very busy today."),
    ("我觉得这个宾馆很好。", "I think this hotel is very good."),
    ("天太热了，我想喝点儿冷的。", "It's too hot, I want something cold to drink."),
    ("他生病了，今天不能来上班。", "He's sick and can't come to work today."),
    ("我们一起准备晚上的饭吧。", "Let's prepare dinner together."),
    ("你的手表很漂亮，是新买的吗？", "Your watch is nice, is it newly bought?"),
    ("这条路我走过很多次。", "I've walked this road many times."),
    ("快考试了，他每天都在学习。", "The exam is coming, he studies every day."),
    ("我已经在这儿工作三年了。", "I have been working here for three years."),
    ("他正在房间里看书。", "He is reading in his room."),
    ("到了机场，请给我打个电话。", "When you get to the airport, please call me."),
    ("我每天早上都看一会儿报纸。", "I read the newspaper for a while every morning."),
    ("这个题我想了很长时间。", "I thought about this problem for a long time."),
    ("我们坐在一起，喝茶说话。", "We sit together, drinking tea and chatting."),
    ("他给我介绍了一个新朋友。", "He introduced me to a new friend."),
    ("天气好的时候，我喜欢出去走走。", "When the weather is good, I like to go out for a walk."),
    ("学习虽然累，但是我很快乐。", "Studying is tiring, but I'm happy."),
]

# --- Final HSK2 batch ---
HSK2 += [
    ("请问，从这儿到学校远不远？", "Excuse me, is it far from here to the school?"),
    ("我哥哥比我大，但是我比他高。", "My older brother is older than me, but I'm taller."),
    ("他每天都要喝两杯咖啡。", "He drinks two cups of coffee every day."),
    ("这个手机太贵了，我买不起。", "This phone is too expensive, I can't afford it."),
    ("等天气好了，我们一起去旅游。", "When the weather gets nice, let's go traveling together."),
    ("我妹妹唱歌跳舞都很好。", "My younger sister is good at both singing and dancing."),
    ("吃饭以前要先洗手。", "You should wash your hands before eating."),
    ("我每天工作八个小时。", "I work eight hours every day."),
    ("他笑着说：欢迎你们！", "He said with a smile: Welcome!"),
    ("时间过得真快，已经五月了。", "Time flies, it's already May."),
    ("我们的新教室很大，也很新。", "Our new classroom is big and new."),
    ("这次考试我准备得很好。", "I prepared well for this exam."),
    ("他身体很好，因为他常常运动。", "He is healthy because he exercises often."),
    ("我想买点儿水果送给老师。", "I want to buy some fruit to give to the teacher."),
    ("明天有汉语课，你要早点儿来。", "There's Chinese class tomorrow, come early."),
    ("他从公司回家要一个小时。", "It takes him an hour to get home from the company."),
    ("我觉得你今天看起来很高兴。", "I think you look very happy today."),
    ("天气冷了，要多穿衣服。", "It's getting cold, wear more clothes."),
    ("我们认识已经十年了。", "We have known each other for ten years now."),
    ("谢谢大家，今天我真的很快乐。", "Thank you everyone, I'm really happy today."),
]


# --- Extra HSK2 batch (to reach 500) ---
HSK2 += [
    ("他比我大，可是没有我高。", "He is older than me, but not as tall."),
    ("我每天坐公共汽车上班，要一个小时。", "I take the bus to work every day; it takes an hour."),
    ("你的生日是几月几号？", "What month and day is your birthday?"),
    ("我的生日是十月一号。", "My birthday is October first."),
    ("外面阴天了，可能要下雨。", "It's cloudy outside, it might rain."),
    ("他笑着说：欢迎，欢迎！", "He smiled and said: Welcome, welcome!"),
    ("我们公司离机场很近。", "Our company is close to the airport."),
    ("这件事我真的不知道。", "I really don't know about this."),
    ("快开始了，请大家坐好。", "It's about to start, please everyone be seated."),
    ("他每天都准备得很认真。", "He prepares very seriously every day."),
    ("这本书很有意思，你也看看吧。", "This book is interesting, you should read it too."),
    ("天气这么好，我们出去走走吧。", "The weather is so nice, let's go out for a walk."),
    ("他给我介绍了一个公司。", "He introduced me to a company."),
    ("我想问问你，这个怎么走？", "Let me ask you, how do I get there?"),
    ("您要的牛奶和鸡蛋都买好了。", "The milk and eggs you wanted are all bought."),
    ("我已经在这家公司工作两年了。", "I've already worked at this company for two years."),
    ("他身体不太好，常常生病。", "He isn't very healthy and often gets sick."),
    ("我们一起准备晚上的考试吧。", "Let's prepare for tonight's exam together."),
    ("这件红色的衣服真好看。", "This red piece of clothing looks really nice."),
    ("他跑步跑得很快，比我快多了。", "He runs very fast, much faster than me."),
    ("吃完早饭，我就去上班。", "After breakfast, I go to work."),
    ("姐姐唱歌，妹妹跳舞，大家都很高兴。", "Sister sings, younger sister dances, everyone is happy."),
    ("我希望明年能去中国旅游。", "I hope I can travel to China next year."),
    ("天黑了，孩子们还在外面玩。", "It's dark, but the children are still playing outside."),
    ("他对人很好，大家都喜欢他。", "He is nice to people, and everyone likes him."),
    ("这个手机不贵，也很漂亮。", "This phone isn't expensive and is pretty."),
    ("我们坐在旁边，等他下课。", "We sit to the side, waiting for him to finish class."),
    ("请问，从这儿到您的公司远吗？", "Excuse me, is it far from here to your company?"),
    ("他工作很忙，但是身体很好。", "He is busy with work, but very healthy."),
    ("谢谢您今天来，请慢走。", "Thank you for coming today, take care."),
]


# ============ Integrated Chinese Level 1 (Lessons 1–20) ============
# Sentences based on the dialogues of each lesson, staying within the
# lesson's vocabulary and following the textbook storyline
# (Wang Peng, Li You, Gao Wenzhong, Bai Ying'ai, ...).
# Format: (lesson number, theme, [(chinese, english), ...])

IC1 = [
    (1, "Greetings", [
        ("你好！", "Hello!"),
        ("请问，你贵姓？", "Excuse me, may I ask your family name?"),
        ("我姓李。你呢？", "My family name is Li. And you?"),
        ("我姓王，叫王朋。", "My family name is Wang; I'm called Wang Peng."),
        ("你叫什么名字？", "What's your name?"),
        ("我叫李友。", "My name is Li You."),
        ("李小姐，你是老师吗？", "Miss Li, are you a teacher?"),
        ("我不是老师，我是学生。", "I'm not a teacher; I'm a student."),
        ("你也是学生吗？", "Are you a student too?"),
        ("我也是学生。", "I'm a student too."),
        ("王先生，你是中国人吗？", "Mr. Wang, are you Chinese?"),
        ("我是中国人，我是北京人。", "I'm Chinese; I'm from Beijing."),
        ("你是美国人吗？", "Are you American?"),
        ("我是美国人，我是纽约人。", "I'm American; I'm from New York."),
        ("李友是学生，王朋也是学生。", "Li You is a student, and Wang Peng is a student too."),
    ]),
    (2, "Family", [
        ("高文中，那是你的照片吗？", "Gao Wenzhong, is that your photo?"),
        ("这是我爸爸，这是我妈妈。", "This is my dad, and this is my mom."),
        ("这个女孩子是谁？", "Who is this girl?"),
        ("她是我姐姐。", "She is my older sister."),
        ("这个男孩子是你弟弟吗？", "Is this boy your younger brother?"),
        ("他不是我弟弟，他是我大哥的儿子。", "He isn't my younger brother; he's my eldest brother's son."),
        ("你大哥有女儿吗？", "Does your eldest brother have a daughter?"),
        ("他没有女儿。", "He doesn't have a daughter."),
        ("白英爱，你家有几口人？", "Bai Ying'ai, how many people are in your family?"),
        ("我家有六口人。", "There are six people in my family."),
        ("你爸爸妈妈做什么工作？", "What work do your mom and dad do?"),
        ("我爸爸是律师。", "My dad is a lawyer."),
        ("我妈妈是英文老师。", "My mom is an English teacher."),
        ("我哥哥、妹妹都是大学生。", "My older brother and younger sister are both college students."),
        ("我妈妈也是老师，我爸爸是医生。", "My mom is also a teacher; my dad is a doctor."),
    ]),
    (3, "Time", [
        ("九月十二号是星期四。", "September 12th is a Thursday."),
        ("那天是我的生日。", "That day is my birthday."),
        ("你今年多大？", "How old are you this year?"),
        ("我今年十八岁。", "I'm eighteen this year."),
        ("星期四我请你吃晚饭，怎么样？", "Thursday I'll treat you to dinner — how about it?"),
        ("太好了！谢谢你。", "Great! Thank you."),
        ("你喜欢吃中国菜还是美国菜？", "Do you like Chinese food or American food?"),
        ("我是英国人，可是我喜欢吃中国菜。", "I'm British, but I like Chinese food."),
        ("现在几点？", "What time is it now?"),
        ("现在六点半。", "It's six thirty now."),
        ("我们今天晚上七点半见。", "Let's meet tonight at seven thirty."),
        ("你今天晚上忙不忙？", "Are you busy tonight?"),
        ("我今天很忙，可是明天不忙。", "I'm busy today, but not tomorrow."),
        ("我今天晚上有事儿。", "I have something to do tonight."),
        ("你认识不认识李友？", "Do you know Li You?"),
        ("我认识她，她是我的同学。", "I know her; she's my classmate."),
    ]),
    (4, "Hobbies", [
        ("白英爱，你周末喜欢做什么？", "Bai Ying'ai, what do you like to do on weekends?"),
        ("我喜欢打球、看电视。你呢？", "I like playing ball and watching TV. And you?"),
        ("我喜欢唱歌、跳舞，还喜欢听音乐。", "I like singing and dancing, and I also like listening to music."),
        ("你也喜欢看书，对不对？", "You also like reading, right?"),
        ("对，有的时候也喜欢看书。", "Right, sometimes I like reading too."),
        ("你喜欢不喜欢看电影？", "Do you like watching movies?"),
        ("喜欢，我周末常常看电影。", "I do — I often watch movies on weekends."),
        ("我们今天晚上去看一个外国电影，怎么样？", "Let's go see a foreign movie tonight — how about it?"),
        ("高文中，你想不想去打球？", "Gao Wenzhong, do you want to go play ball?"),
        ("打球？我不喜欢打球。", "Play ball? I don't like playing ball."),
        ("那我们去看球，怎么样？", "Then let's go watch a ball game — how about that?"),
        ("看球？我觉得看球也没有意思。", "Watch a game? I think watching a game is boring too."),
        ("那你这个周末想做什么？", "Then what do you want to do this weekend?"),
        ("我只想吃饭、睡觉。", "I just want to eat and sleep."),
        ("算了，我去找别人。", "Forget it — I'll go find someone else."),
    ]),
    (5, "Visiting Friends", [
        ("谁呀？", "Who is it?"),
        ("是我，王朋，还有李友。", "It's me, Wang Peng — and Li You too."),
        ("请进，请进，快进来！", "Come in, come in! Come on in!"),
        ("李友，这是我姐姐，高小音。", "Li You, this is my older sister, Gao Xiaoyin."),
        ("认识你们我很高兴。", "I'm very happy to meet you."),
        ("你们家很大，也很漂亮。", "Your home is big and very pretty."),
        ("你们想喝点儿什么？", "What would you like to drink?"),
        ("喝茶还是喝咖啡？", "Tea or coffee?"),
        ("我喝茶吧。", "I'll have tea."),
        ("我要一瓶可乐，可以吗？", "I'd like a bottle of cola — is that okay?"),
        ("对不起，我们家没有可乐。", "Sorry, we don't have cola at home."),
        ("那给我一杯水吧。", "Then give me a glass of water."),
        ("昨天晚上，王朋和李友去高文中家玩儿。", "Last night, Wang Peng and Li You went to Gao Wenzhong's home for a visit."),
        ("高小音在学校的图书馆工作。", "Gao Xiaoyin works at the school library."),
        ("王朋喝了两杯茶，李友只喝了一杯水。", "Wang Peng drank two cups of tea; Li You only drank one glass of water."),
        ("他们一起聊天儿、看电视。", "They chatted and watched TV together."),
        ("王朋和李友晚上十二点才回家。", "Wang Peng and Li You didn't go home until twelve at night."),
    ]),
    (6, "Making Appointments", [
        ("喂，请问，常老师在吗？", "Hello, is Teacher Chang there, please?"),
        ("我就是。您是哪位？", "Speaking. Who is this?"),
        ("常老师，您好，我是李友。", "Hello, Teacher Chang — this is Li You."),
        ("今天下午您有时间吗？我想问您几个问题。", "Do you have time this afternoon? I'd like to ask you a few questions."),
        ("对不起，今天下午我要开会。", "Sorry, this afternoon I have a meeting."),
        ("明天上午我有两节课。", "Tomorrow morning I have two classes."),
        ("下午三点我要给二年级考试。", "At three in the afternoon I have to give the second-year students an exam."),
        ("您什么时候有空儿？", "When are you free?"),
        ("我四点以后才有空儿。", "I'm not free until after four."),
        ("要是您方便，四点半我到您的办公室去，行吗？", "If it's convenient for you, I'll come to your office at four thirty — is that all right?"),
        ("没问题，我在办公室等你。", "No problem — I'll wait for you in my office."),
        ("谢谢您！别客气。", "Thank you! Don't mention it."),
        ("下个星期我要考中文。", "Next week I have a Chinese exam."),
        ("你帮我准备一下，好吗？", "Help me prepare a bit, okay?"),
        ("请你跟我说中文，好吗？", "Please speak Chinese with me, okay?"),
        ("好啊，但是你得请我喝咖啡。", "Sure, but you have to treat me to coffee."),
    ]),
    (7, "Studying Chinese", [
        ("李友，你上个星期考试考得怎么样？", "Li You, how did you do on last week's exam?"),
        ("因为你帮我复习，所以考得不错。", "Because you helped me review, I did pretty well."),
        ("我写中国字写得太慢了！", "I write Chinese characters too slowly!"),
        ("以后我跟你一起练习写字，好不好？", "From now on I'll practice writing characters with you, okay?"),
        ("那太好了！我们现在就写，怎么样？", "That's great! Let's write right now — how about it?"),
        ("给我一支笔、一张纸。", "Give me a pen and a sheet of paper."),
        ("你教我怎么写这个字吧。", "Teach me how to write this character."),
        ("你写字写得真漂亮。", "You write characters really beautifully."),
        ("你平常来得很早，今天怎么这么晚？", "You usually come very early — why so late today?"),
        ("我昨天预习中文，早上四点才睡觉。", "I was previewing the Chinese lesson yesterday and didn't go to bed until four in the morning."),
        ("我昨天十点就睡了。", "I went to bed at ten last night."),
        ("今天的语法很容易，生词也不多。", "Today's grammar is easy, and there aren't many new words either."),
        ("我觉得第七课的语法有一点儿难。", "I think the grammar in Lesson Seven is a little hard."),
        ("你说中文说得真好。", "You speak Chinese really well."),
        ("学中文很有意思。", "Studying Chinese is very interesting."),
    ]),
    (8, "School Life", [
        ("我早上七点半起床。", "I get up at seven thirty in the morning."),
        ("洗了澡以后，我就吃早饭。", "After showering, I eat breakfast right away."),
        ("我常常一边吃饭，一边听录音。", "I often listen to recordings while I eat."),
        ("我九点到教室去上课。", "At nine I go to the classroom for class."),
        ("第一节课是中文，老师教我们发音、生词和语法。", "The first class is Chinese; the teacher teaches us pronunciation, new words, and grammar."),
        ("这篇课文很有意思。", "This text is very interesting."),
        ("第二节是电脑课，很难。", "The second class is computer class — it's hard."),
        ("中午我和同学们一起到餐厅去吃午饭。", "At noon I go to the cafeteria with my classmates to eat lunch."),
        ("我们一边吃，一边练习说中文。", "We practice speaking Chinese while we eat."),
        ("下午我到图书馆去上网。", "In the afternoon I go to the library to use the internet."),
        ("四点王朋来找我打球。", "At four, Wang Peng came looking for me to play ball."),
        ("我到的时候，她正在做功课。", "When I arrived, she was doing homework."),
        ("睡觉以前，高文中给我打了一个电话。", "Before I went to bed, Gao Wenzhong gave me a call."),
        ("他告诉我明天要考试。", "He told me there's an exam tomorrow."),
        ("除了专业课以外，我还得学中文。", "Besides my major courses, I also have to study Chinese."),
        ("下个星期六我们学校有一个音乐会，希望你能来。", "Next Saturday there's a concert at our school — I hope you can come."),
    ]),
    (9, "Shopping", [
        ("小姐，您要买什么衣服？", "Miss, what clothes would you like to buy?"),
        ("我想买一件衬衫。", "I'd like to buy a shirt."),
        ("您喜欢什么颜色的，黄的还是红的？", "What color do you like — yellow or red?"),
        ("我喜欢红的。", "I like the red one."),
        ("我还想买一条裤子。", "I'd also like to buy a pair of pants."),
        ("您穿多大的？大号的、中号的，还是小号的？", "What size do you wear? Large, medium, or small?"),
        ("中号的，不要太贵的，也不要太便宜的。", "Medium — not too expensive, but not too cheap either."),
        ("这条裤子怎么样？", "How about this pair of pants?"),
        ("一共多少钱？", "How much altogether?"),
        ("衬衫二十一块五，裤子三十二块九毛九。", "The shirt is twenty-one fifty; the pants are thirty-two ninety-nine."),
        ("一共五十四块四毛九分。", "Altogether that's fifty-four forty-nine."),
        ("对不起，这双鞋太小了，能不能换一双？", "Excuse me, these shoes are too small — can I exchange them for another pair?"),
        ("不行，这双跟那双一样大。", "That won't do — this pair is the same size as that pair."),
        ("这双鞋大小合适，可是颜色不好。", "This pair fits well, but the color isn't good."),
        ("有没有咖啡色的？", "Do you have brown ones?"),
        ("对不起，这种鞋只有黑的。", "Sorry, this kind of shoe only comes in black."),
        ("这双鞋样子挺好的，就买这双吧。", "These shoes look pretty good — I'll take this pair."),
    ]),
    (10, "Transportation", [
        ("李友，寒假你回家吗？", "Li You, are you going home for winter break?"),
        ("对，我要回家。", "Yes, I'm going home."),
        ("飞机票你买了吗？", "Have you bought your plane ticket?"),
        ("已经买了，是二十一号的。", "I've already bought it — it's for the 21st."),
        ("飞机是几点的？", "What time is the flight?"),
        ("晚上八点的。", "Eight in the evening."),
        ("你怎么去机场？", "How are you getting to the airport?"),
        ("我想坐公共汽车或者坐地铁。", "I'm thinking of taking the bus or the subway."),
        ("你先坐汽车，坐三站下车，然后换地铁。", "First take the bus, get off after three stops, then change to the subway."),
        ("先坐红线，再换绿线。", "First take the red line, then change to the green line."),
        ("不行，不行，太麻烦了，我还是打车吧。", "No, no — too much trouble. I'd better just take a taxi."),
        ("出租汽车太贵了。", "Taxis are too expensive."),
        ("我开车送你去机场吧。", "Let me drive you to the airport."),
        ("谢谢你开车送我到机场。", "Thank you for driving me to the airport."),
        ("让你花了那么多时间，真不好意思。", "I made you spend so much time — I'm really embarrassed."),
        ("这儿的人开车开得特别快。", "People here drive especially fast."),
        ("我在高速公路上开车，真有点儿紧张。", "Driving on the highway, I get really a bit nervous."),
        ("这儿没有公共汽车，也没有地铁，只能自己开车，很不方便。", "There are no buses or subway here — you can only drive yourself. It's very inconvenient."),
    ]),
    (11, "Weather", [
        ("今天天气比昨天好，不下雪了。", "The weather today is better than yesterday — it's stopped snowing."),
        ("明天天气怎么样？", "What will the weather be like tomorrow?"),
        ("我在网上看了天气预报。", "I checked the weather forecast online."),
        ("明天天气比今天更好。", "Tomorrow's weather will be even better than today's."),
        ("不但不会下雪，而且会暖和一点儿。", "Not only won't it snow, it will even be a bit warmer."),
        ("我约了朋友明天去公园滑冰。", "I've arranged with a friend to go ice-skating in the park tomorrow."),
        ("怎么又下雨了？", "How come it's raining again?"),
        ("这儿的天气真糟糕。", "The weather here is really awful."),
        ("下个星期我要去纽约面试。", "Next week I'm going to New York for an interview."),
        ("这儿冬天很冷，夏天很热。", "Here, winter is very cold and summer is very hot."),
        ("加州冬天不冷，夏天不热。", "In California, winter isn't cold and summer isn't hot."),
        ("加州春天暖和，秋天凉快。", "In California, spring is warm and autumn is cool."),
        ("今天天气这么好，我们出去玩儿吧。", "The weather is so nice today — let's go out and have some fun."),
        ("你看，外面又下雨了。", "Look, it's raining again outside."),
        ("天气预报说明天会下雨。", "The forecast says it will rain tomorrow."),
    ]),
    (12, "Dining", [
        ("请问，现在还有没有位子？", "Excuse me, are there any seats left?"),
        ("好像一个位子都没有了。", "It seems there isn't a single seat left."),
        ("服务员，点菜！", "Waiter, we'd like to order!"),
        ("你们想吃点儿什么？", "What would you like to eat?"),
        ("我要一盘饺子，要素的。", "I'd like a plate of dumplings — vegetarian ones."),
        ("再来一盘家常豆腐，不要放肉。", "Also a plate of home-style tofu — don't put any meat in it."),
        ("我们都吃素。", "We're both vegetarian."),
        ("还要两碗酸辣汤，不要放味精，少放点儿盐。", "We also want two bowls of hot-and-sour soup — no MSG, and easy on the salt."),
        ("对不起，白菜刚卖完。", "Sorry, we just sold out of cabbage."),
        ("再来两杯冰茶，我渴死了。", "And two glasses of iced tea — I'm dying of thirst."),
        ("我饿死了，请上菜快一点儿。", "I'm starving — please bring the food a little faster."),
        ("师傅，今天晚饭有什么好吃的？", "Sir, what's good for dinner today?"),
        ("糖醋鱼甜甜的、酸酸的，好吃极了。", "The sweet-and-sour fish is sweet and tangy — extremely tasty."),
        ("红烧牛肉刚卖完。", "The soy-braised beef just sold out."),
        ("那就来一个凉拌黄瓜吧。", "Then I'll have a cucumber salad."),
        ("再来一碗米饭。", "And a bowl of rice, please."),
    ]),
    (13, "Directions", [
        ("请问，你上哪儿去？", "Excuse me, where are you headed?"),
        ("我想去学校的电脑中心。", "I want to go to the school's computer center."),
        ("你知道怎么走吗？", "Do you know how to get there?"),
        ("听说电脑中心在运动场旁边。", "I've heard the computer center is next to the sports field."),
        ("电脑中心离这儿远不远？", "Is the computer center far from here?"),
        ("不远，离这儿很近。", "Not far — it's very close to here."),
        ("你住的地方离运动场很近。", "The place you live is very close to the sports field."),
        ("我没去过中国城，不知道中国城在哪儿。", "I've never been to Chinatown; I don't know where it is."),
        ("没问题，你开车，我告诉你怎么走。", "No problem — you drive and I'll tell you how to go."),
        ("你有地图吗？", "Do you have a map?"),
        ("从这儿一直往南开。", "From here, drive straight south."),
        ("过三个路口，往西一拐就到了。", "Pass three intersections, turn west, and you're there."),
        ("前面红绿灯往右拐。", "Turn right at the traffic light ahead."),
        ("到了，到了，你看，前面有很多中国字。", "We're here! Look — there are lots of Chinese characters up ahead."),
        ("那个地方我去过很多次。", "I've been to that place many times."),
    ]),
    (14, "Birthday Party", [
        ("李友，你做什么呢？", "Li You, what are you doing?"),
        ("我在做功课呢。", "I'm doing homework."),
        ("今天晚上我们给小音过生日。", "Tonight we're celebrating Xiaoyin's birthday."),
        ("我们晚上在她家开舞会。", "This evening we're having a party at her place."),
        ("我带什么东西？", "What should I bring?"),
        ("你带一些饮料和水果吧。", "Bring some drinks and fruit."),
        ("我们先吃饭，吃完饭再唱歌跳舞。", "We'll eat first; after eating we'll sing and dance."),
        ("你住的地方离小音家远吗？", "Is your place far from Xiaoyin's home?"),
        ("我开车来接你吧。", "I'll come pick you up in my car."),
        ("小音，祝你生日快乐！", "Xiaoyin, happy birthday!"),
        ("这是我们给你买的礼物。", "This is the gift we bought for you."),
        ("谢谢你们！快进来吧。", "Thank you! Come on in."),
        ("她是王朋的妹妹王红，刚从北京来。", "She's Wang Peng's younger sister, Wang Hong — she just came from Beijing."),
        ("你属什么？", "What's your zodiac animal?"),
        ("我属狗。", "I was born in the year of the dog."),
        ("她长得真可爱。", "She's really cute."),
        ("她的眼睛大大的，很像她妈妈。", "Her eyes are big — she looks a lot like her mother."),
    ]),
    (15, "Seeing a Doctor", [
        ("你怎么了？哪儿不舒服？", "What's wrong? Where does it hurt?"),
        ("我肚子疼死了。", "My stomach is killing me."),
        ("你昨天吃什么东西了？", "What did you eat yesterday?"),
        ("我吃了一些剩菜。", "I ate some leftovers."),
        ("你吃坏肚子了。", "You've eaten something that upset your stomach."),
        ("要不要打针？", "Do I need an injection?"),
        ("我不想打针，吃药吧。", "I don't want a shot — let's just take medicine."),
        ("这种药一天吃三次，一次两片。", "Take this medicine three times a day, two tablets each time."),
        ("多喝水，多休息。", "Drink more water and rest more."),
        ("你的眼睛怎么红红的？", "Why are your eyes all red?"),
        ("我可能对什么过敏了。", "I might be allergic to something."),
        ("赶快去看医生吧。", "Hurry and go see a doctor."),
        ("我没有健康保险，看病太贵了。", "I don't have health insurance — seeing a doctor is too expensive."),
        ("你不去看病，病会越来越重。", "If you don't get it treated, the illness will get worse and worse."),
        ("我上次生病，没吃药也好了。", "Last time I was sick, I got better without taking any medicine."),
    ]),
    (16, "Dating", [
        ("这个周末学校演一个中国电影，我们一起去看，好吗？", "This weekend the school is showing a Chinese movie — shall we go see it together?"),
        ("听说看电影的人很多，买得到票吗？", "I hear a lot of people go to the movies — can we get tickets?"),
        ("票已经买了，我费了很大的力气才买到。", "I've already bought the tickets — it took a lot of effort to get them."),
        ("好极了！我早就想看这个电影了。", "Fantastic! I've wanted to see this movie for a long time."),
        ("还有别人跟我们一起去吗？", "Is anyone else going with us?"),
        ("没有，就我们俩。", "No, just the two of us."),
        ("我请你吃晚饭，吃完饭以后我们去看电影。", "I'll treat you to dinner, and after dinner we'll go see the movie."),
        ("那太好了，星期六见！", "That's great — see you Saturday!"),
        ("白小姐，你还记得我吗？", "Miss Bai, do you still remember me?"),
        ("对不起，你是哪位？", "Sorry — who is this?"),
        ("我想请你周末去跳舞。", "I'd like to invite you to go dancing this weekend."),
        ("这个周末不行，我要打扫房子、整理房间。", "This weekend won't work — I have to clean the house and tidy my room."),
        ("下个周末怎么样？", "How about next weekend?"),
        ("对不起，下个周末我也没有时间。", "Sorry, I don't have time next weekend either."),
    ]),
    (17, "Renting an Apartment", [
        ("王朋在学校的宿舍住了两个学期了。", "Wang Peng has lived in the school dorm for two semesters."),
        ("他觉得宿舍太吵，房间太小。", "He thinks the dorm is too noisy and the room too small."),
        ("房间太小，连电脑都放不下。", "The room is so small there isn't even space for a computer."),
        ("宿舍里没有地方做饭，很不方便。", "There's nowhere to cook in the dorm — it's very inconvenient."),
        ("他想搬出去住。", "He wants to move out."),
        ("你看报纸上的广告了吗？", "Did you see the ad in the newspaper?"),
        ("请问，你们有公寓出租吗？", "Excuse me, do you have an apartment for rent?"),
        ("有啊，一房一厅，还带家具。", "We do — one bedroom and one living room, and it comes furnished."),
        ("客厅里有一套沙发和一张饭桌。", "In the living room there's a sofa set and a dining table."),
        ("卧室里有一张床、一张书桌和一个书架。", "In the bedroom there's a bed, a desk, and a bookshelf."),
        ("你们那里安静不安静？", "Is it quiet where you are?"),
        ("非常安静。", "Very quiet."),
        ("一个月房租多少钱？", "How much is the rent per month?"),
        ("八百五十块，水电费不用付。", "Eight hundred fifty — you don't pay for water or electricity."),
        ("可以养宠物吗？", "Can I keep pets?"),
        ("对不起，我们这儿不许养宠物。", "Sorry, pets aren't allowed here."),
    ]),
    (18, "Sports", [
        ("你看，我的肚子越来越大了。", "Look, my belly is getting bigger and bigger."),
        ("你平常吃得那么多，又不运动，当然越来越胖了。", "You usually eat so much and never exercise — of course you're getting fatter."),
        ("那怎么办呢？", "Then what should I do?"),
        ("最简单的运动是跑步。", "The simplest exercise is running."),
        ("冬天那么冷，夏天那么热，跑步多难受啊！", "Winter is so cold and summer so hot — running is so miserable!"),
        ("打网球得买网球拍、网球鞋，太贵了。", "To play tennis you have to buy a racket and tennis shoes — too expensive."),
        ("我们去游泳吧。", "Let's go swimming."),
        ("游泳？我怕水，太危险了。", "Swimming? I'm afraid of water — too dangerous."),
        ("为了提高中文水平，我们以后多说中文吧。", "To improve our Chinese, let's speak more Chinese from now on."),
        ("你在看什么呢？", "What are you watching?"),
        ("我在看美式足球赛。", "I'm watching an American football game."),
        ("足球不是圆的吗？", "Isn't a football round?"),
        ("美式足球可以用手抱着球跑。", "In American football you can run holding the ball in your hands."),
        ("你看，那个人又被压在下面了。", "Look, that guy got crushed at the bottom of the pile again."),
    ]),
    (19, "Travel", [
        ("马上要放假了，这个暑假你有什么计划？", "Vacation is almost here — what are your plans for the summer?"),
        ("我想回家看父母。", "I want to go home and see my parents."),
        ("你去过北京吗？", "Have you been to Beijing?"),
        ("北京是中国的首都，也是中国的政治、文化中心。", "Beijing is China's capital, and also its political and cultural center."),
        ("北京有很多名胜古迹。", "Beijing has many famous scenic spots and historic sites."),
        ("我给你当导游，怎么样？", "I'll be your tour guide — how about it?"),
        ("太好了！可是我还没有护照呢。", "Great! But I don't have a passport yet."),
        ("你得赶快办护照，还要办签证。", "You need to get a passport right away, and a visa too."),
        ("我们找一家旅行社订机票吧。", "Let's find a travel agency to book the plane tickets."),
        ("请问，六月初到北京的机票多少钱？", "Excuse me, how much is a ticket to Beijing in early June?"),
        ("你要单程票还是往返票？", "Do you want one-way or round-trip?"),
        ("往返的多少钱？", "How much is round-trip?"),
        ("有没有直飞的？", "Is there a direct flight?"),
        ("要不要转机？", "Do I have to change planes?"),
        ("我帮您查一下，这家航空公司的票最便宜。", "Let me check for you — this airline's tickets are the cheapest."),
    ]),
    (20, "At the Airport", [
        ("小姐，这是我们的机票和护照。", "Miss, here are our tickets and passports."),
        ("你们有几件行李要托运？", "How many pieces of luggage do you have to check?"),
        ("这个包不托运，我们带上飞机。", "We're not checking this bag — we'll carry it onto the plane."),
        ("行李超重了吗？", "Is the luggage overweight?"),
        ("没有，没超重。", "No, it's not overweight."),
        ("这是你们的登机牌。", "Here are your boarding passes."),
        ("请到五号登机口上飞机。", "Please board at Gate 5."),
        ("飞机几点起飞？", "What time does the plane take off?"),
        ("别哭了，我们秋天就回来。", "Don't cry — we'll be back in the fall."),
        ("你们要多保重！", "Take good care of yourselves!"),
        ("到了北京以后，别忘了给我们打电话。", "After you get to Beijing, don't forget to call us."),
        ("路上小心！", "Be careful on the way!"),
        ("爸、妈，我们回来了！", "Dad, Mom, we're back!"),
        ("我来介绍一下，这是我的同学李友。", "Let me introduce — this is my classmate Li You."),
        ("欢迎你来北京！路上辛苦了。", "Welcome to Beijing! You must be tired from the trip."),
        ("走，我们回家吧，妈给你们做了很多好吃的。", "Come on, let's go home — Mom has made you lots of good food."),
        ("太好了，我要吃北京烤鸭！", "Wonderful — I want to eat Beijing roast duck!"),
    ]),
]


# ===================== Spoken — everyday colloquial sentences =====================
# What people actually say, as opposed to textbook prose: short, high-frequency
# utterances for real situations (reacting, texting, ordering, taxis, WeChat).
# Kept beginner-friendly — mostly HSK 1/2 vocabulary plus unavoidable daily-life
# words (微信, 外卖, 师傅, 老板…). Entries are (chinese, english) or
# (chinese, english, usage_note) — the note is shown on the card back.

SPOKEN = []

# --- Instant replies & reactions ---
SPOKEN += [
    ("好的。", "Okay."),
    ("行。", "Sure, that works."),
    ("没问题。", "No problem."),
    ("好啊！", "Sure!"),
    ("可以。", "That's fine."),
    ("不行。", "That won't work."),
    ("真的吗？", "Really?"),
    ("真的假的？", "For real?",
     "Literally \"true or fake?\" — a very common way to react to surprising news."),
    ("不会吧！", "No way!"),
    ("太好了！", "That's great!"),
    ("太棒了！", "Awesome!"),
    ("当然可以。", "Of course you can."),
    ("都行。", "Either way works for me."),
    ("随便，你定吧。", "Whatever — you decide."),
    ("算了。", "Forget it."),
    ("没事儿。", "It's fine, no worries."),
    ("别担心。", "Don't worry."),
    ("放心吧。", "Don't worry, I've got it."),
    ("加油！", "You've got this!",
     "Literally \"add oil\" — all-purpose encouragement: sports, exams, hard days."),
    ("慢慢来，不着急。", "Take your time, no rush."),
]

# --- Greetings & small talk ---
SPOKEN += [
    ("好久不见！", "Long time no see!"),
    ("最近怎么样？", "How've you been lately?"),
    ("还行吧。", "Not bad, I guess."),
    ("挺好的。", "Pretty good."),
    ("老样子。", "Same as always."),
    ("吃了吗？", "Have you eaten?",
     "A classic friendly greeting — it asks how you're doing, not whether you want food."),
    ("忙什么呢？", "What are you busy with?"),
    ("我先走了。", "I'm off."),
    ("慢走！", "Take care!",
     "Said to someone leaving — literally \"walk slowly\". Shopkeepers say it to customers."),
    ("回头见。", "See you later."),
    ("路上小心。", "Be safe getting there."),
    ("早点儿休息。", "Get some rest — don't stay up late."),
    ("周末愉快！", "Have a good weekend!"),
    ("替我问他好。", "Tell him I said hi."),
    ("你今天过得怎么样？", "How was your day?"),
    ("好久没见你了。", "I haven't seen you in ages."),
    ("下班了吗？", "Are you off work yet?"),
]

# --- Understanding each other ---
SPOKEN += [
    ("你说什么？", "What did you say?"),
    ("再说一遍，好吗？", "Say that again, okay?"),
    ("你说慢点儿。", "Slow down a bit."),
    ("我没听懂。", "I didn't catch that."),
    ("我听不懂。", "I don't understand."),
    ("你懂我的意思吗？", "You know what I mean?"),
    ("懂了懂了。", "Got it, got it."),
    ("什么意思？", "What does that mean?"),
    ("这个用中文怎么说？", "How do you say this in Chinese?"),
    ("这个字怎么念？", "How do you read this character?"),
    ("我的中文不太好。", "My Chinese isn't great."),
    ("你说得太快了。", "You're talking too fast."),
    ("能写下来吗？", "Can you write it down?"),
    ("我没听清楚。", "I didn't hear that clearly."),
    ("你是说明天吗？", "You mean tomorrow?"),
]

# --- Eating out ---
SPOKEN += [
    ("老板，来一碗面。", "One bowl of noodles, please.",
     "老板 (\"boss\") is how you address shopkeepers and small-restaurant owners."),
    ("你们这儿有什么好吃的？", "What's good here?"),
    ("再来一杯。", "Another glass, please."),
    ("我不吃辣。", "I don't eat spicy food."),
    ("微辣就行。", "Mild spicy is fine."),
    ("不要香菜。", "No cilantro."),
    ("在这儿吃还是带走？", "For here or to go?"),
    ("带走，谢谢。", "To go, thanks."),
    ("帮我打包一下。", "Could you box this up for me?"),
    ("服务员，买单！", "Check, please!"),
    ("一共多少钱啊？", "How much all together?"),
    ("这次我请客。", "This one's on me."),
    ("下次你请。", "Next time you're paying."),
    ("好吃吗？", "Is it good?"),
    ("太好吃了！", "So good!"),
    ("我吃饱了。", "I'm stuffed."),
    ("先来点儿喝的吧。", "Let's start with drinks."),
    ("你随便点，别客气。", "Order whatever you like, don't be shy."),
    ("这家店人真多。", "This place is packed."),
    ("还有位子吗？", "Any seats left?"),
]

# --- Shopping & paying ---
SPOKEN += [
    ("老板，这个多少钱？", "How much is this one?"),
    ("太贵了，便宜点儿吧。", "Too expensive — knock the price down a bit."),
    ("能不能再便宜一点儿？", "Can you go any lower?"),
    ("我随便看看。", "I'm just browsing."),
    ("就要这个吧。", "I'll take this one."),
    ("有别的颜色吗？", "Do you have other colors?"),
    ("我能试试吗？", "Can I try it?"),
    ("有小一点儿的吗？", "Got a smaller one?"),
    ("可以刷卡吗？", "Can I pay by card?"),
    ("用微信可以吗？", "Is WeChat okay?"),
    ("扫这个码就行。", "Just scan this code."),
    ("我用支付宝。", "I'll use Alipay."),
    ("不用了，谢谢。", "No thanks, I'm good."),
    ("我再想想。", "Let me think about it."),
    ("这个卖完了。", "This one's sold out."),
]

# --- Getting around ---
SPOKEN += [
    ("请问，卫生间在哪儿？", "Excuse me, where's the bathroom?"),
    ("附近有地铁站吗？", "Is there a subway station nearby?"),
    ("离这儿远不远？", "Is it far from here?"),
    ("走路能到吗？", "Can I walk there?"),
    ("大概要多长时间？", "About how long will it take?"),
    ("师傅，去机场，谢谢。", "To the airport please, thanks.",
     "师傅 (\"master\") is how you address drivers, repairmen, and other workers."),
    ("师傅，停这儿就行。", "You can just stop here."),
    ("前面右拐。", "Turn right up ahead."),
    ("一直走就到了。", "Keep going straight and you're there."),
    ("我迷路了。", "I'm lost."),
    ("我到了，你在哪儿呢？", "I'm here — where are you?"),
    ("我们在哪儿下车？", "Where do we get off?"),
    ("下一站就到了。", "It's the next stop."),
    ("今天堵车堵得厉害。", "Traffic is terrible today."),
    ("师傅，慢点儿开。", "Not so fast, please (to a driver)."),
]

# --- Phones & making plans ---
SPOKEN += [
    ("喂，你在哪儿呢？", "Hey, where are you?"),
    ("我马上到。", "I'll be right there."),
    ("我快到了。", "I'm almost there."),
    ("再等我十分钟。", "Give me ten more minutes."),
    ("你到了叫我。", "Let me know when you get here."),
    ("我在路上呢。", "I'm on my way."),
    ("你先吃，别等我。", "Start without me — don't wait."),
    ("今天晚上有空吗？", "Are you free tonight?"),
    ("出来玩儿吧！", "Come out and hang!"),
    ("明天再说吧。", "Let's figure it out tomorrow."),
    ("改天吧。", "Some other day, maybe."),
    ("下次一定。", "Next time for sure.",
     "Often a polite way of saying no — the promised \"next time\" may never come."),
    ("时间你定。", "You pick the time."),
    ("老地方见。", "See you at the usual place."),
    ("别迟到啊！", "Don't be late!"),
    ("我可能会晚一点儿。", "I might be a little late."),
    ("到家了给我发个消息。", "Text me when you're home."),
    ("我手机快没电了。", "My phone's about to die."),
]

# --- Feelings & states ---
SPOKEN += [
    ("累死了！", "I'm dead tired!",
     "死了 after an adjective means \"extremely\" — 累死了, 饿死了, 热死了 are everywhere in speech."),
    ("饿死了！", "I'm starving!"),
    ("热死了！", "It's boiling!"),
    ("冷死了！", "It's freezing!"),
    ("我困死了。", "I'm so sleepy."),
    ("我有点儿累，想回家了。", "I'm a bit tired — I want to head home."),
    ("我先睡了，晚安。", "I'm off to bed — night."),
    ("好无聊啊。", "I'm so bored."),
    ("烦死了！", "So annoying!"),
    ("吓我一跳！", "You scared me!"),
    ("太可惜了。", "What a shame."),
    ("好可爱啊！", "So cute!"),
    ("我好紧张啊。", "I'm so nervous."),
    ("别紧张，没事的。", "Relax, it'll be fine."),
    ("我今天心情不太好。", "I'm not in a great mood today."),
    ("今天玩儿得真开心。", "Today was really fun."),
    ("我想家了。", "I miss home."),
    ("你怎么了？不开心吗？", "What's wrong? You seem down."),
]

# --- Courtesy & help ---
SPOKEN += [
    ("麻烦你了。", "Sorry for the trouble — thanks."),
    ("辛苦了！", "Thanks for your hard work!",
     "Acknowledges someone's effort — said to coworkers, delivery drivers, anyone who just worked."),
    ("不好意思，打扰一下。", "Sorry to bother you."),
    ("借过一下。", "Excuse me, coming through."),
    ("帮我个忙，好吗？", "Do me a favor, will you?"),
    ("帮我拿一下这个。", "Hold this for me a sec."),
    ("谢谢啦！", "Thanks!"),
    ("不用谢。", "Don't mention it."),
    ("客气什么呀。", "No need to be so polite."),
    ("你先请。", "After you."),
    ("没事，我来吧。", "It's fine — I'll do it."),
    ("你人真好。", "You're so nice."),
    ("哪里哪里。", "Oh, I'm flattered.",
     "Humble reply to a compliment — literally \"where, where?\""),
    ("是我不好，对不起啊。", "My bad — sorry."),
]

# --- Around the house ---
SPOKEN += [
    ("我出门了！", "I'm heading out!"),
    ("我到家了。", "I'm home."),
    ("我先回去了。", "I'm heading back."),
    ("该起床了！", "Time to get up!"),
    ("该睡觉了。", "Time for bed."),
    ("我去洗个澡。", "I'm gonna take a shower."),
    ("饭好了，快来吃！", "Food's ready — come eat!"),
    ("今天我做饭。", "I'm cooking today."),
    ("帮我开一下门。", "Get the door for me."),
    ("把灯关了吧。", "Turn the light off."),
    ("空调开一下，太热了。", "Turn on the AC — it's too hot."),
    ("我出去买点儿东西。", "I'm going out to grab a few things."),
    ("家里没吃的了。", "There's no food in the house."),
    ("点个外卖吧。", "Let's just order delivery."),
    ("你在干什么呢？", "What are you up to?"),
    ("没干什么，在家躺着呢。", "Nothing much — just lying around at home."),
]

# --- Opinions & reactions ---
SPOKEN += [
    ("我觉得还行。", "I think it's okay."),
    ("一般般吧。", "It's just so-so."),
    ("还不错！", "Pretty good!"),
    ("这个特别好吃。", "This one's really good."),
    ("没想到啊！", "I did not see that coming!"),
    ("我也这么觉得。", "That's what I think too."),
    ("你说得对。", "You're right."),
    ("好主意！", "Good idea!"),
    ("听你的。", "Your call."),
    ("应该没问题。", "Should be fine."),
    ("我不太清楚。", "I'm not too sure."),
    ("谁知道呢。", "Who knows."),
    ("反正我不去。", "Either way, I'm not going."),
    ("说真的，我有点儿担心。", "Honestly, I'm a little worried."),
    ("那就这么定了。", "Then it's settled."),
    ("看情况吧。", "Depends — we'll see."),
]

# --- Phones, photos, WeChat ---
SPOKEN += [
    ("加个微信吧。", "Let's add each other on WeChat."),
    ("你扫我吧。", "You scan mine (my QR code)."),
    ("发我微信。", "Send it to me on WeChat."),
    ("我把位置发给你。", "I'll send you the location."),
    ("麻烦帮我们拍张照。", "Could you take a photo of us?"),
    ("再拍一张！", "Take one more!"),
    ("这张拍得真好。", "This one came out great."),
    ("我手机没电了。", "My phone died."),
    ("你有充电器吗？", "Do you have a charger?"),
    ("这儿信号不太好。", "The signal's not great here."),
    ("密码是多少？", "What's the password?"),
    ("我看到你发的了。", "I saw what you sent."),
    ("别玩儿手机了！", "Get off your phone!"),
    ("我在刷手机呢。", "I'm just scrolling on my phone."),
    ("拍好了吗？", "Did you get the shot?"),
    ("到了给我打电话。", "Call me when you're there."),
]


# ===================== Build output =====================

def _item(n, zh, en, note=None):
    row = {"n": n, "zh": zh, "py": to_pinyin(zh), "en": en}
    notes = ([note] if note else []) + polyphone_notes(zh)
    if notes:
        row["notes"] = notes
    return row


def build(pairs):
    return [_item(i, *entry) for i, entry in enumerate(pairs, start=1)]


def build_ic1():
    """Flat sentence list plus per-lesson index ranges (1-based, inclusive)."""
    items, lessons = [], []
    n = 1
    for num, theme, sentences in IC1:
        start = n
        for zh, en in sentences:
            items.append(_item(n, zh, en))
            n += 1
        lessons.append({"n": num, "title": theme, "from": start, "to": n - 1})
    return items, lessons


def main():
    ic1_items, ic1_lessons = build_ic1()
    data = {
        "hsk1": build(HSK1),
        "hsk2": build(HSK2),
        "spoken": build(SPOKEN),
        "ic1": ic1_items,
        "ic1_chapters": ic1_lessons,
    }
    body = json.dumps(data, ensure_ascii=False, indent=2)
    js = ("// Auto-generated by tools/gen_sentences.py — do not edit by hand.\n"
          "// Pinyin produced with pypinyin (tone marks).\n"
          "window.SENTENCE_DATA = " + body + ";\n")
    with open(OUT, "w", encoding="utf-8") as f:
        f.write(js)
    print("Wrote", OUT)
    print("HSK1:", len(data["hsk1"]), "HSK2:", len(data["hsk2"]),
          "Spoken:", len(data["spoken"]),
          "IC1:", len(ic1_items), "sentences in", len(ic1_lessons), "lessons")


if __name__ == "__main__":
    main()
