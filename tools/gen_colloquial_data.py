# -*- coding: utf-8 -*-
"""
Generate the data file for the Colloquial Character Trainer.

BASE500 is the top 500 characters from Jun Da's Modern Chinese Character
Frequency List (via github.com/ruddfawcett/hanziDB.csv), in frequency-rank
order with their standard pinyin reading.

ENTRIES holds the hand-authored content for the characters that have been
turned into flashcards so far: a short colloquial gloss and one natural,
chat-register example sentence (+ English translation). Not every rank has
an entry yet -- only ranks present in ENTRIES are emitted, so the trainer
grows as more batches are added here. Sentence pinyin is generated with
pypinyin so tone marks stay accurate and consistent (reusing the tone-sandhi
and polyphone handling from gen_sentences.py).

Run:  python3 tools/gen_colloquial_data.py
Out:  pages/colloquial_data.js
"""

import json
import os
import re

from pypinyin import pinyin, Style

HERE = os.path.dirname(os.path.abspath(__file__))
OUT = os.path.join(HERE, "..", "pages", "colloquial_data.js")

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
    """Return list of (char, syllable_or_None), one contiguous Han run at a
    time so phrase context is preserved, then aligned 1:1 with the string."""
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

    for idx, (ch, s) in enumerate(pairs):
        if s is None:
            continue
        if ch == "地" and idx >= 2 and pairs[idx - 1][0] == pairs[idx - 2][0]:
            s = "de"                     # adverbial particle after a reduplicated adjective (慢慢地)
        elif ch == "长":
            nxt_ch = pairs[idx + 1][0] if idx + 1 < len(pairs) else ""
            s = "zhǎng" if nxt_ch in ("高", "大") else "cháng"   # 长高/长大 = grow
        elif ch == "都" and s == "dū":
            s = "dōu"
        elif ch == "子" and s in ("zǐ", "zī"):
            s = "zi"
        elif ch == "好" and s == "hào":
            s = "hǎo"
        elif ch == "得":
            nxt_ch = pairs[idx + 1][0] if idx + 1 < len(pairs) else ""
            if pairs[idx - 1:idx] and pairs[idx - 1][0] == "我" and nxt_ch == "走":
                s = "děi"
            elif nxt_ch == "到":
                s = "dé"                 # 得到 = obtain
            else:
                s = "de"
        pairs[idx] = (ch, s)

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
                s = "yī"
            else:
                s = "yí" if _tone(nxt) == 4 else "yì"
        else:
            continue
        pairs[idx] = (ch, s)

    return pairs


def to_pinyin(zh: str) -> str:
    """Tone-marked pinyin with polyphone corrections, 不/一 tone sandhi and
    erhua. Word-spaced, punctuation kept inline."""
    pairs = _corrected(zh)

    toks = []
    for idx, (ch, s) in enumerate(pairs):
        if ch == "儿" and s is not None:
            prev_ch = pairs[idx - 1][0] if idx > 0 else ""
            next_ch = pairs[idx + 1][0] if idx + 1 < len(pairs) else ""
            if prev_ch == "女" or next_ch == "子":
                toks.append(s)
            elif toks and re.search(r"[a-zü]", toks[-1]):
                toks[-1] = toks[-1] + "r"
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
    text = text.replace("duì bù qǐ", "duì bu qǐ")
    return text


# ===================== Tone-change notes =====================
# A card gets a short note when its character's tone actually shifts in
# context: the 一 and 不 sandhi rules, 3rd tone before another 3rd tone,
# or a polyphone read differently in the example than the citation pinyin.

_3RD_TO_2ND = str.maketrans("ǎěǐǒǔǚ", "áéíóúǘ")


def _next_pair(pairs, idx):
    """First (char, syllable) after idx that is an actual Han syllable."""
    for ch, s in pairs[idx + 1:]:
        if s is not None:
            return ch, s
    return None, None


def tone_note(char: str, citation_py: str, ex: str) -> str | None:
    pairs = _corrected(ex)
    idx = next((i for i, (ch, s) in enumerate(pairs)
                if ch == char and s is not None), None)
    if idx is None:
        return None
    syl = pairs[idx][1]
    nxt_ch, nxt = _next_pair(pairs, idx)

    if char == "一":
        here = f"here 一{nxt_ch} is {syl} {nxt}" if nxt else f"here it's {syl}"
        return ("Tone change: 一 is yī alone or in numbers/dates, "
                "yí before a 4th tone, yì before 1st/2nd/3rd tones — "
                + here + ".")
    if char == "不":
        here = f"here 不{nxt_ch} is {syl} {nxt}" if nxt else f"here it's {syl}"
        return ("Tone change: 不 becomes bú before a 4th tone, "
                "otherwise bù — " + here + ".")
    # 3rd-tone sandhi needs directly adjacent syllables (no pause between).
    adj_ch, adj = pairs[idx + 1] if idx + 1 < len(pairs) else (None, None)
    if _tone(syl) == 3 and adj and _tone(adj) == 3:
        spoken = syl.translate(_3RD_TO_2ND)
        return (f"Tone change: a 3rd tone before another 3rd tone is "
                f"pronounced 2nd tone — {char}{adj_ch} is written "
                f"{syl} {adj} but said {spoken} {adj}.")
    if syl != citation_py:
        return (f"Polyphone: usually read {citation_py}, "
                f"but {syl} in this sentence.")
    return None


# ===================== BASE500: (rank, character, pinyin) =====================
# Jun Da frequency rank 1-500, simplified characters, standard pinyin.

BASE500 = [
    (1, "的", "de"), (2, "一", "yī"), (3, "是", "shì"), (4, "不", "bù"), (5, "了", "le"),
    (6, "在", "zài"), (7, "人", "rén"), (8, "有", "yǒu"), (9, "我", "wǒ"), (10, "他", "tā"),
    (11, "这", "zhè"), (12, "个", "gè"), (13, "们", "men"), (14, "中", "zhōng"), (15, "来", "lái"),
    (16, "上", "shàng"), (17, "大", "dà"), (18, "为", "wèi"), (19, "和", "hé"), (20, "国", "guó"),
    (21, "地", "de"), (22, "到", "dào"), (23, "以", "yǐ"), (24, "说", "shuō"), (25, "时", "shí"),
    (26, "要", "yào"), (27, "就", "jiù"), (28, "出", "chū"), (29, "会", "huì"), (30, "可", "kě"),
    (31, "也", "yě"), (32, "你", "nǐ"), (33, "对", "duì"), (34, "生", "shēng"), (35, "能", "néng"),
    (36, "而", "ér"), (37, "子", "zi"), (38, "那", "nà"), (39, "得", "dé"), (40, "于", "yú"),
    (41, "着", "zhe"), (42, "下", "xià"), (43, "自", "zì"), (44, "之", "zhī"), (45, "年", "nián"),
    (46, "过", "guò"), (47, "发", "fā"), (48, "后", "hòu"), (49, "作", "zuò"), (50, "里", "lǐ"),
    (51, "用", "yòng"), (52, "道", "dào"), (53, "行", "xíng"), (54, "所", "suǒ"), (55, "然", "rán"),
    (56, "家", "jiā"), (57, "种", "zhǒng"), (58, "事", "shì"), (59, "成", "chéng"), (60, "方", "fāng"),
    (61, "多", "duō"), (62, "经", "jīng"), (63, "么", "me"), (64, "去", "qù"), (65, "法", "fǎ"),
    (66, "学", "xué"), (67, "如", "rú"), (68, "都", "dōu"), (69, "同", "tóng"), (70, "现", "xiàn"),
    (71, "当", "dāng"), (72, "没", "méi"), (73, "动", "dòng"), (74, "面", "miàn"), (75, "起", "qǐ"),
    (76, "看", "kàn"), (77, "定", "dìng"), (78, "天", "tiān"), (79, "分", "fēn"), (80, "还", "hái"),
    (81, "进", "jìn"), (82, "好", "hǎo"), (83, "小", "xiǎo"), (84, "部", "bù"), (85, "其", "qí"),
    (86, "些", "xiē"), (87, "主", "zhǔ"), (88, "样", "yàng"), (89, "理", "lǐ"), (90, "心", "xīn"),
    (91, "她", "tā"), (92, "本", "běn"), (93, "前", "qián"), (94, "开", "kāi"), (95, "但", "dàn"),
    (96, "因", "yīn"), (97, "只", "zhǐ"), (98, "从", "cóng"), (99, "想", "xiǎng"), (100, "实", "shí"),
    (101, "日", "rì"), (102, "军", "jūn"), (103, "者", "zhě"), (104, "意", "yì"), (105, "无", "wú"),
    (106, "力", "lì"), (107, "它", "tā"), (108, "与", "yǔ"), (109, "长", "zhǎng"), (110, "把", "bǎ"),
    (111, "机", "jī"), (112, "十", "shí"), (113, "民", "mín"), (114, "第", "dì"), (115, "公", "gōng"),
    (116, "此", "cǐ"), (117, "已", "yǐ"), (118, "工", "gōng"), (119, "使", "shǐ"), (120, "情", "qíng"),
    (121, "明", "míng"), (122, "性", "xìng"), (123, "知", "zhī"), (124, "全", "quán"), (125, "三", "sān"),
    (126, "又", "yòu"), (127, "关", "guān"), (128, "点", "diǎn"), (129, "正", "zhèng"), (130, "业", "yè"),
    (131, "外", "wài"), (132, "将", "jiāng"), (133, "两", "liǎng"), (134, "高", "gāo"), (135, "间", "jiān"),
    (136, "由", "yóu"), (137, "问", "wèn"), (138, "很", "hěn"), (139, "最", "zuì"), (140, "重", "zhòng"),
    (141, "并", "bìng"), (142, "物", "wù"), (143, "手", "shǒu"), (144, "应", "yīng"), (145, "战", "zhàn"),
    (146, "向", "xiàng"), (147, "头", "tóu"), (148, "文", "wén"), (149, "体", "tǐ"), (150, "政", "zhèng"),
    (151, "美", "měi"), (152, "相", "xiāng"), (153, "见", "jiàn"), (154, "被", "bèi"), (155, "利", "lì"),
    (156, "什", "shén"), (157, "二", "èr"), (158, "等", "děng"), (159, "产", "chǎn"), (160, "或", "huò"),
    (161, "新", "xīn"), (162, "己", "jǐ"), (163, "制", "zhì"), (164, "身", "shēn"), (165, "果", "guǒ"),
    (166, "加", "jiā"), (167, "西", "xī"), (168, "斯", "sī"), (169, "月", "yuè"), (170, "话", "huà"),
    (171, "合", "hé"), (172, "回", "huí"), (173, "特", "tè"), (174, "代", "dài"), (175, "内", "nèi"),
    (176, "信", "xìn"), (177, "表", "biǎo"), (178, "化", "huà"), (179, "老", "lǎo"), (180, "给", "gěi"),
    (181, "世", "shì"), (182, "位", "wèi"), (183, "次", "cì"), (184, "度", "dù"), (185, "门", "mén"),
    (186, "任", "rèn"), (187, "常", "cháng"), (188, "先", "xiān"), (189, "海", "hǎi"), (190, "通", "tōng"),
    (191, "教", "jiào"), (192, "儿", "ér"), (193, "原", "yuán"), (194, "东", "dōng"), (195, "声", "shēng"),
    (196, "提", "tí"), (197, "立", "lì"), (198, "及", "jí"), (199, "比", "bǐ"), (200, "员", "yuán"),
    (201, "解", "jiě"), (202, "水", "shuǐ"), (203, "名", "míng"), (204, "真", "zhēn"), (205, "论", "lùn"),
    (206, "处", "chù"), (207, "走", "zǒu"), (208, "义", "yì"), (209, "各", "gè"), (210, "入", "rù"),
    (211, "几", "jǐ"), (212, "口", "kǒu"), (213, "认", "rèn"), (214, "条", "tiáo"), (215, "平", "píng"),
    (216, "系", "xì"), (217, "气", "qì"), (218, "题", "tí"), (219, "活", "huó"), (220, "尔", "ěr"),
    (221, "更", "gèng"), (222, "别", "bié"), (223, "打", "dǎ"), (224, "女", "nǚ"), (225, "变", "biàn"),
    (226, "四", "sì"), (227, "神", "shén"), (228, "总", "zǒng"), (229, "何", "hé"), (230, "电", "diàn"),
    (231, "数", "shù"), (232, "安", "ān"), (233, "少", "shǎo"), (234, "报", "bào"), (235, "才", "cái"),
    (236, "结", "jié"), (237, "反", "fǎn"), (238, "受", "shòu"), (239, "目", "mù"), (240, "太", "tài"),
    (241, "量", "liàng"), (242, "再", "zài"), (243, "感", "gǎn"), (244, "建", "jiàn"), (245, "务", "wù"),
    (246, "做", "zuò"), (247, "接", "jiē"), (248, "必", "bì"), (249, "场", "chǎng"), (250, "件", "jiàn"),
    (251, "计", "jì"), (252, "管", "guǎn"), (253, "期", "qī"), (254, "市", "shì"), (255, "直", "zhí"),
    (256, "德", "dé"), (257, "资", "zī"), (258, "命", "mìng"), (259, "山", "shān"), (260, "金", "jīn"),
    (261, "指", "zhǐ"), (262, "克", "kè"), (263, "许", "xǔ"), (264, "统", "tǒng"), (265, "区", "qū"),
    (266, "保", "bǎo"), (267, "至", "zhì"), (268, "队", "duì"), (269, "形", "xíng"), (270, "社", "shè"),
    (271, "便", "biàn"), (272, "空", "kōng"), (273, "决", "jué"), (274, "治", "zhì"), (275, "展", "zhǎn"),
    (276, "马", "mǎ"), (277, "科", "kē"), (278, "司", "sī"), (279, "五", "wǔ"), (280, "基", "jī"),
    (281, "眼", "yǎn"), (282, "书", "shū"), (283, "非", "fēi"), (284, "则", "zé"), (285, "听", "tīng"),
    (286, "白", "bái"), (287, "却", "què"), (288, "界", "jiè"), (289, "达", "dá"), (290, "光", "guāng"),
    (291, "放", "fàng"), (292, "强", "qiáng"), (293, "即", "jí"), (294, "像", "xiàng"), (295, "难", "nán"),
    (296, "且", "qiě"), (297, "权", "quán"), (298, "思", "sī"), (299, "王", "wáng"), (300, "象", "xiàng"),
    (301, "完", "wán"), (302, "设", "shè"), (303, "式", "shì"), (304, "色", "sè"), (305, "路", "lù"),
    (306, "记", "jì"), (307, "南", "nán"), (308, "品", "pǐn"), (309, "住", "zhù"), (310, "告", "gào"),
    (311, "类", "lèi"), (312, "求", "qiú"), (313, "据", "jù"), (314, "程", "chéng"), (315, "北", "běi"),
    (316, "边", "biān"), (317, "死", "sǐ"), (318, "张", "zhāng"), (319, "该", "gāi"), (320, "交", "jiāo"),
    (321, "规", "guī"), (322, "万", "wàn"), (323, "取", "qǔ"), (324, "拉", "lā"), (325, "格", "gé"),
    (326, "望", "wàng"), (327, "觉", "jué"), (328, "术", "shù"), (329, "领", "lǐng"), (330, "共", "gòng"),
    (331, "确", "què"), (332, "传", "chuán"), (333, "师", "shī"), (334, "观", "guān"), (335, "清", "qīng"),
    (336, "今", "jīn"), (337, "切", "qiè"), (338, "院", "yuàn"), (339, "让", "ràng"), (340, "识", "shí"),
    (341, "候", "hòu"), (342, "带", "dài"), (343, "导", "dǎo"), (344, "争", "zhēng"), (345, "运", "yùn"),
    (346, "笑", "xiào"), (347, "飞", "fēi"), (348, "风", "fēng"), (349, "步", "bù"), (350, "改", "gǎi"),
    (351, "收", "shōu"), (352, "根", "gēn"), (353, "干", "gàn"), (354, "造", "zào"), (355, "言", "yán"),
    (356, "联", "lián"), (357, "持", "chí"), (358, "组", "zǔ"), (359, "每", "měi"), (360, "济", "jì"),
    (361, "车", "chē"), (362, "亲", "qīn"), (363, "极", "jí"), (364, "林", "lín"), (365, "服", "fú"),
    (366, "快", "kuài"), (367, "办", "bàn"), (368, "议", "yì"), (369, "往", "wǎng"), (370, "元", "yuán"),
    (371, "英", "yīng"), (372, "士", "shì"), (373, "证", "zhèng"), (374, "近", "jìn"), (375, "失", "shī"),
    (376, "转", "zhuǎn"), (377, "夫", "fū"), (378, "令", "lìng"), (379, "准", "zhǔn"), (380, "布", "bù"),
    (381, "始", "shǐ"), (382, "怎", "zěn"), (383, "呢", "ne"), (384, "存", "cún"), (385, "未", "wèi"),
    (386, "远", "yuǎn"), (387, "叫", "jiào"), (388, "台", "tái"), (389, "单", "dān"), (390, "影", "yǐng"),
    (391, "具", "jù"), (392, "罗", "luō"), (393, "字", "zì"), (394, "爱", "ài"), (395, "击", "jī"),
    (396, "流", "liú"), (397, "备", "bèi"), (398, "兵", "bīng"), (399, "连", "lián"), (400, "调", "diào"),
    (401, "深", "shēn"), (402, "商", "shāng"), (403, "算", "suàn"), (404, "质", "zhì"), (405, "团", "tuán"),
    (406, "集", "jí"), (407, "百", "bǎi"), (408, "需", "xū"), (409, "价", "jià"), (410, "花", "huā"),
    (411, "党", "dǎng"), (412, "华", "huá"), (413, "城", "chéng"), (414, "石", "shí"), (415, "级", "jí"),
    (416, "整", "zhěng"), (417, "府", "fǔ"), (418, "离", "lí"), (419, "况", "kuàng"), (420, "亚", "yà"),
    (421, "请", "qǐng"), (422, "技", "jì"), (423, "际", "jì"), (424, "约", "yuē"), (425, "示", "shì"),
    (426, "复", "fù"), (427, "病", "bìng"), (428, "息", "xī"), (429, "究", "jiū"), (430, "线", "xiàn"),
    (431, "似", "shì"), (432, "官", "guān"), (433, "火", "huǒ"), (434, "断", "duàn"), (435, "精", "jīng"),
    (436, "满", "mǎn"), (437, "支", "zhī"), (438, "视", "shì"), (439, "消", "xiāo"), (440, "越", "yuè"),
    (441, "器", "qì"), (442, "容", "róng"), (443, "照", "zhào"), (444, "须", "xū"), (445, "九", "jiǔ"),
    (446, "增", "zēng"), (447, "研", "yán"), (448, "写", "xiě"), (449, "称", "chēng"), (450, "企", "qǐ"),
    (451, "八", "bā"), (452, "功", "gōng"), (453, "吗", "ma"), (454, "包", "bāo"), (455, "片", "piàn"),
    (456, "史", "shǐ"), (457, "委", "wěi"), (458, "乎", "hū"), (459, "查", "chá"), (460, "轻", "qīng"),
    (461, "易", "yì"), (462, "早", "zǎo"), (463, "曾", "céng"), (464, "除", "chú"), (465, "农", "nóng"),
    (466, "找", "zhǎo"), (467, "装", "zhuāng"), (468, "广", "guǎng"), (469, "显", "xiǎn"), (470, "吧", "ba"),
    (471, "阿", "ā"), (472, "李", "lǐ"), (473, "标", "biāo"), (474, "谈", "tán"), (475, "吃", "chī"),
    (476, "图", "tú"), (477, "念", "niàn"), (478, "六", "liù"), (479, "引", "yǐn"), (480, "历", "lì"),
    (481, "首", "shǒu"), (482, "医", "yī"), (483, "局", "jú"), (484, "突", "tū"), (485, "专", "zhuān"),
    (486, "费", "fèi"), (487, "号", "hào"), (488, "尽", "jǐn"), (489, "另", "lìng"), (490, "周", "zhōu"),
    (491, "较", "jiào"), (492, "注", "zhù"), (493, "语", "yǔ"), (494, "仅", "jǐn"), (495, "考", "kǎo"),
    (496, "落", "luò"), (497, "青", "qīng"), (498, "随", "suí"), (499, "选", "xuǎn"), (500, "列", "liè"),
]

BASE_BY_RANK = {r: (c, py) for r, c, py in BASE500}


# ===================== ENTRIES: hand-authored flashcard content =====================
# rank -> {"en": short gloss, "ex": example sentence, "exEn": translation}
# Only ranks present here become flashcards. Add more batches over time.

ENTRIES = {}


def add(rank_start, rows):
    """rows: list of (en, ex, exEn), applied to ranks rank_start, +1, +2, ..."""
    for i, (en, ex, ex_en) in enumerate(rows):
        ENTRIES[rank_start + i] = {"en": en, "ex": ex, "exEn": ex_en}


# --- Batch 1: ranks 1-70 ---
add(1, [
    ("of; 's (possessive particle)", "这是我的手机。", "This is my phone."),
    ("one", "我要一杯咖啡。", "I want one coffee."),
    ("to be; yes", "我是学生。", "I am a student."),
    ("not; no", "我不知道。", "I don't know."),
    ("(completed action / change particle)", "我吃了。", "I've eaten."),
    ("at; in; -ing (progressive)", "我在家。", "I'm at home."),
    ("person; people", "他是好人。", "He's a good person."),
    ("to have; there is", "你有时间吗？", "Do you have time?"),
    ("I; me", "我很好。", "I'm doing well."),
    ("he; him", "他在哪儿？", "Where is he?"),
    ("this", "这个多少钱？", "How much is this?"),
    ("(general measure word)", "给我一个。", "Give me one."),
    ("(plural marker for pronouns/people)", "你们好！", "Hello, everyone!"),
    ("middle; China (abbr.)", "我在中国。", "I'm in China."),
    ("to come", "你快来！", "Come quick!"),
    ("on; above; to go up", "桌子上有一本书。", "There's a book on the table."),
    ("big", "这个太大了。", "This is too big."),
    ("for; because of", "我为你高兴。", "I'm happy for you."),
    ("and; with", "我和你一起去。", "I'll go with you."),
    ("country", "你是哪国人？", "Which country are you from?"),
    ("-ly (adverbial particle)", "他慢慢地走。", "He walked slowly."),
    ("to arrive; to", "我到了。", "I've arrived."),
    ("(in 以为: to think, mistakenly)", "我以为你走了。", "I thought you'd left."),
    ("to say; to speak", "你说什么？", "What did you say?"),
    ("time; when (in 有时)", "我有时会迟到。", "I'm sometimes late."),
    ("to want; will", "我要一杯水。", "I want a glass of water."),
    ("just; then (emphasis)", "我马上就到。", "I'll be there right away."),
    ("to go out; to exit", "我们出去吧。", "Let's go out."),
    ("can; to know how to; will", "你会说中文吗？", "Can you speak Chinese?"),
    ("can; may (in 可以)", "可以进来吗？", "May I come in?"),
    ("also; too", "我也是。", "Me too."),
    ("you", "你好吗？", "How are you?"),
    ("correct; towards", "你说得对。", "You're right."),
    ("life; to be born (in 生日)", "生日快乐！", "Happy birthday!"),
    ("can; to be able to", "我能帮你吗？", "Can I help you?"),
    ("and also (in 而且)", "他又高又壮，而且很聪明。", "He's tall and strong, and also very smart."),
    ("(noun suffix)", "这是我儿子。", "This is my son."),
    ("that", "那是谁？", "Who is that?"),
    ("to get; to obtain", "我得到了好消息。", "I got good news."),
    ("(formal: in, at; in 取决于)", "这取决于你。", "It depends on you."),
    ("(progressive aspect particle)", "我在等着你。", "I'm waiting for you."),
    ("under; below; next", "下雨了。", "It's raining."),
    ("self (in 自己)", "我自己来。", "I'll do it myself."),
    ("(formal connector, in 总之)", "总之，我同意。", "In short, I agree."),
    ("year", "明年见！", "See you next year!"),
    ("to pass; (experienced-action particle)", "我去过北京。", "I've been to Beijing."),
    ("to send; to issue", "我发给你了。", "I sent it to you."),
    ("after; behind", "放学后见。", "See you after school."),
    ("to do; to make (in 工作)", "我在工作。", "I'm working."),
    ("inside; in", "我在家里。", "I'm at home (inside)."),
    ("to use", "你会用这个吗？", "Do you know how to use this?"),
    ("way; to say (in 知道)", "我知道了。", "Got it."),
    ("OK; to walk; to be alright", "行，就这么办。", "OK, let's do it that way."),
    ("place (in 所以)", "所以我没去。", "So I didn't go."),
    ("so; like that (in 然后)", "然后呢？", "And then what?"),
    ("home; family", "我要回家了。", "I'm heading home."),
    ("kind; type; to plant", "这种事很常见。", "This kind of thing is common."),
    ("matter; thing; affair", "没事儿。", "It's nothing; no worries."),
    ("to become; to succeed", "我们成功了！", "We did it!"),
    ("square; place (in 地方)", "这个地方真漂亮。", "This place is really pretty."),
    ("many; much", "你多大了？", "How old are you?"),
    ("to pass through (in 已经)", "我已经吃了。", "I've already eaten."),
    ("(question suffix, in 什么/怎么)", "你在干什么？", "What are you doing?"),
    ("to go", "你要去哪儿？", "Where are you going?"),
    ("method (in 办法)", "有什么办法吗？", "Is there any way to solve this?"),
    ("to study; to learn", "我在学中文。", "I'm learning Chinese."),
    ("if; like (in 如果)", "如果你忙，就别来了。", "If you're busy, don't bother coming."),
    ("all; both", "我们都到了。", "We've all arrived."),
    ("same; together (in 同学)", "我们是同学。", "We're classmates."),
])

# --- Batch 2: ranks 70-140 ---
add(70, [
    ("now (in 现在)", "你现在在哪儿？", "Where are you right now?"),
    ("of course (in 当然); to serve as", "那当然！", "Of course!"),
    ("not (have); did not", "我没听懂。", "I didn't understand."),
    ("to move; to act", "别动！", "Don't move!"),
    ("face; side; noodles", "我们见面聊吧。", "Let's meet up and talk."),
    ("to rise; up (in 一起)", "我们一起去吧。", "Let's go together."),
    ("to look; to watch; to read", "你在看什么？", "What are you watching?"),
    ("surely (in 一定); to decide", "我一定会去。", "I will definitely go."),
    ("day; sky", "今天天气真好。", "The weather's really nice today."),
    ("to divide; minute; point", "还有五分钟。", "There are five more minutes."),
    ("still; also; even more", "你还好吗？", "Are you okay?"),
    ("to enter", "请进！", "Please come in!"),
    ("good; well; OK", "好的，没问题！", "OK, no problem!"),
    ("small; little", "这只是小事。", "It's just a small thing."),
    ("part; unit (measure word for films)", "这部电影很好看。", "This movie is really good."),
    ("actually (in 其实)", "其实我不太喜欢。", "Actually, I don't really like it."),
    ("some; a few", "你要不要吃一些？", "Do you want to eat some?"),
    ("main (in 主意)", "这是个好主意！", "That's a good idea!"),
    ("kind; way (in 这样/怎么样)", "你觉得怎么样？", "What do you think?"),
    ("reason; sense (in 道理)", "你说得有道理。", "What you said makes sense."),
    ("heart; mind", "别担心。", "Don't worry."),
    ("she; her", "她是谁？", "Who is she?"),
    ("originally (in 本来)", "我本来想去。", "I originally wanted to go."),
    ("front; before; ago", "你以前来过吗？", "Have you been here before?"),
    ("to open; to start; to drive", "快开门！", "Open the door, quick!"),
    ("but; however", "我很累，但是很开心。", "I'm tired, but happy."),
    ("because (of) (in 因为)", "因为下雨，我没去。", "I didn't go because it was raining."),
    ("only; just", "我只是问问。", "I'm just asking."),
    ("from", "你从哪儿来？", "Where are you from?"),
    ("to think; to want; to miss", "我想你了。", "I miss you."),
    ("real; true (in 其实/说实话)", "说实话，我不知道。", "To be honest, I don't know."),
    ("sun; day", "今天是几月几日？", "What's today's date?"),
    ("army; military", "他当过军人。", "He used to be a soldier."),
    ("(suffix: one who; or)", "你去或者我去，都行。", "Either you go or I go, both are fine."),
    ("meaning; idea (in 意思)", "这是什么意思？", "What does this mean?"),
    ("without; not (formal, in 无所谓)", "无所谓，随便你。", "Whatever, up to you."),
    ("strength; effort (in 努力)", "你要努力！", "You need to work hard!"),
    ("it", "它是我的猫。", "It's my cat."),
    ("and; with (formal)", "这与我无关。", "This has nothing to do with me."),
    ("to grow; long", "你长高了！", "You've grown taller!"),
    ("(object-marker); to hold", "把门关上。", "Close the door."),
    ("machine (in 手机)", "我手机没电了。", "My phone's out of battery."),
    ("ten", "我要十个饺子。", "I'll have ten dumplings."),
    ("people; citizens (in 移民)", "移民很不容易。", "Immigrating isn't easy."),
    ("(ordinal prefix)", "这是我第一次来。", "This is my first time here."),
    ("public (in 公司)", "我在公司加班。", "I'm working overtime at the office."),
    ("this (formal, in 因此)", "因此，我们决定取消。", "So, we decided to cancel."),
    ("already (in 已经)", "我已经到了。", "I've already arrived."),
    ("work; labor", "这个工作很有意思。", "This job is interesting."),
    ("to make; even if (in 即使)", "即使很累，我也要去。", "Even if I'm tired, I'll still go."),
    ("feeling; mood (in 心情)", "你今天心情怎么样？", "How are you feeling today?"),
    ("clear; to understand (in 明白)", "我明白了。", "I understand now."),
    ("nature; character (in 性格)", "他性格很好。", "He has a good personality."),
    ("to know (in 知道)", "你知道吗？", "Do you know?"),
    ("whole; entire", "我们全都同意。", "We all agree."),
    ("three", "我们三个人一起去。", "The three of us will go together."),
    ("again; also", "你又迟到了！", "You're late again!"),
    ("to close; related (in 没关系)", "没关系。", "No worries; it's okay."),
    ("o'clock; a bit; dot", "几点了？", "What time is it?"),
    ("exactly; right now (in 正在)", "我正在吃饭。", "I'm eating right now."),
    ("profession (in 作业/毕业)", "你做完作业了吗？", "Have you finished your homework?"),
    ("outside; foreign", "外面在下雨。", "It's raining outside."),
    ("will (in 将来)", "我将来想当医生。", "In the future, I want to be a doctor."),
    ("two (measure use)", "我要两杯奶茶。", "I want two milk teas."),
    ("tall; high", "他很高。", "He's tall."),
    ("room (in 房间)", "我们的房间在哪儿？", "Where's our room?"),
    ("due to (in 由于)", "由于堵车，我迟到了。", "I was late due to traffic."),
    ("to ask", "我可以问你一个问题吗？", "Can I ask you a question?"),
    ("very", "今天很热。", "It's very hot today."),
    ("most; -est", "这是我最喜欢的歌。", "This is my favorite song."),
    ("heavy; important (in 重要)", "这件事很重要。", "This matter is very important."),
])


def build():
    out = []
    for rank, chpy in sorted(BASE_BY_RANK.items()):
        entry = ENTRIES.get(rank)
        if entry is None:
            continue
        char, py = chpy
        row = {
            "rank": rank,
            "c": char,
            "py": py,
            "en": entry["en"],
            "ex": entry["ex"],
            "exPy": to_pinyin(entry["ex"]),
            "exEn": entry["exEn"],
        }
        note = tone_note(char, py, entry["ex"])
        if note:
            row["tone"] = note
        out.append(row)
    return out


def main():
    data = build()
    with open(OUT, "w", encoding="utf-8") as f:
        f.write("// Auto-generated by tools/gen_colloquial_data.py — do not edit by hand.\n")
        f.write("// Character + pinyin from Jun Da's frequency list; examples hand-authored,\n")
        f.write("// example pinyin produced with pypinyin (tone marks).\n")
        f.write("window.COLLOQUIAL_DATA = ")
        f.write(json.dumps(data, ensure_ascii=False, indent=2))
        f.write(";\n")
    print(f"Wrote {len(data)} entries to {OUT}")


if __name__ == "__main__":
    main()
