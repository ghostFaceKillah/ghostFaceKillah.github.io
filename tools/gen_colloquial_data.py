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

LESSONS adds a "mini lesson" to the grammar workhorses (的, 可, 了, 得, ...)
whose single gloss + example can't cover their real range: each lesson lists
the character's distinct everyday uses with a pattern, gloss and a tiny
example sentence of its own.

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
        elif ch == "地" and idx + 1 < len(pairs) and pairs[idx + 1][0] in "笑说走看哭":
            s = "de"                     # adverbial particle before a verb (开心地笑)
        elif ch == "长":
            nxt_ch = pairs[idx + 1][0] if idx + 1 < len(pairs) else ""
            s = "zhǎng" if nxt_ch in ("高", "大", "得") else "cháng"   # 长高/长大/长得 = grow
        elif ch == "都" and s == "dū" and not (idx and pairs[idx - 1][0] == "首"):
            s = "dōu"                    # 首都 (capital) keeps dū
        elif ch == "子" and s in ("zǐ", "zī"):
            s = "zi"
        elif ch in "妈爸哥姐弟妹奶爷" and idx and pairs[idx - 1][0] == ch:
            s = {"妈": "ma", "爸": "ba", "哥": "ge", "姐": "jie",
                 "弟": "di", "妹": "mei", "奶": "nai", "爷": "ye"}[ch]
            # reduplicated kinship terms: second syllable is neutral (妈妈 mā ma)
        elif ch == "好" and s == "hào" and not (idx and pairs[idx - 1][0] == "爱"):
            s = "hǎo"                    # 爱好 (hobby) keeps hào
        elif ch == "只" and idx and pairs[idx - 1][0] in "一两三几每半":
            s = "zhī"                    # measure word after a number (两只猫)
        elif ch == "着" and idx and pairs[idx - 1][0] == "不":
            s = "zháo"                   # potential complement (睡不着)
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


# ===================== MNEMONICS: char -> (parts, mnem) =====================
# Same style as the Word Trainer: a component-breakdown line and a 💡 memory
# aid (HTML, <b>/<i> allowed). Mnemonics are memory aids, not always strict
# etymology; sound-hinting components are flagged (≈ sound).

MNEMONICS = {
    "的": ("白 white + 勺 ladle",
           "A <b>white</b> (白) <b>ladle</b> (勺) — whose is it? Mark the owner with 的: 我的 = mine."),
    "一": ("one stroke",
           "<b>One</b> single stroke — the simplest character there is."),
    "是": ("日 sun over a straight base",
           "Under the <b>sun</b> (日) everything stands straight and true — “it <b>is</b> so.”"),
    "不": ("pictograph: a root blocked by the ground",
           "A seed's root hits solid rock and can't sprout — <b>no</b>, <b>not</b> happening."),
    "了": ("子 child without arms",
           "A <b>child</b> (子) swaddled with its arms wrapped in — nothing left to do: done, finished, 了."),
    "在": ("才 sprout + 土 earth",
           "A sprout (才) rooted in the <b>earth</b> (土) — it exists somewhere: <b>at</b>, <b>in</b>."),
    "人": ("pictograph: two legs walking",
           "Two legs mid-stride — a <b>person</b> walking."),
    "有": ("𠂇 hand + 月 meat",
           "A <b>hand</b> (𠂇) holding <b>meat</b> (月) — you <b>have</b> something."),
    "我": ("contains 戈 dagger-axe",
           "<b>I</b> grip my <b>spear</b> (戈) and plant my feet — “me first, this is mine.”"),
    "他": ("亻 person + 也 also",
           "That <b>person</b> (亻) over there is <b>also</b> (也) one of us — <b>he</b>."),
    "这": ("辶 walk + 文 writing",
           "<b>Walk</b> (辶) over to the <b>writing</b> (文) and point right at it — <b>this</b> one."),
    "个": ("人 person + 丨 stick",
           "A person holding one bamboo <b>stick</b> (丨) — one generic <b>item</b>: the all-purpose measure word."),
    "们": ("亻 person + 门 door (≈ <b>mén</b>, sound)",
           "A crowd of <b>people</b> (亻) at the <b>door</b> (门) — the plural marker: 我们, 你们."),
    "中": ("口 box + 丨 line through the center",
           "An arrow through the dead <b>center</b> of the target — <b>middle</b>."),
    "来": ("pictograph: a wheat plant",
           "Ripe wheat means guests <b>come</b> for the harvest — 来!"),
    "上": ("a mark above the base line",
           "A flag planted <b>above</b> the ground line — <b>up</b>, on top."),
    "大": ("人 person with arms stretched wide",
           "A person stretching their arms as wide as they can — “it was THIS <b>big</b>!”"),
    "为": ("from 爲: a hand leading an elephant",
           "Originally a hand leading an elephant to work — effort <b>for</b> a purpose: 为你 for you."),
    "和": ("禾 grain + 口 mouth",
           "<b>Grain</b> (禾) in every <b>mouth</b> (口) — harmony: this <b>and</b> that, shared together."),
    "国": ("囗 border + 玉 jade",
           "<b>Jade</b> (玉) treasure locked inside secure <b>borders</b> (囗) — a <b>country</b>."),
    "地": ("土 earth + 也",
           "<b>Earth</b> (土) is the base of everything — the ground; as a particle it grounds adverbs: 慢慢地 slowly."),
    "到": ("至 arrive + 刂 knife (≈ <b>dāo</b>, sound)",
           "The arrow <b>arrives</b> (至) and the knife (刂) pins the exact spot — you've <b>arrived</b>."),
    "以": ("a person picking up a tool",
           "Pick up the tool and work <b>by means of</b> it; 以为 = take it (wrongly) to be so."),
    "说": ("讠 speech + 兑 exchange",
           "<b>Speech</b> (讠) <b>exchanged</b> (兑) back and forth — to <b>speak</b>."),
    "时": ("日 sun + 寸 inch",
           "Measuring the <b>sun</b> (日) <b>inch</b> (寸) by inch across the sky — <b>time</b>."),
    "要": ("西 west + 女 woman",
           "The <b>woman</b> (女) from the <b>west</b> (西) knows exactly what she <b>wants</b>."),
    "就": ("京 capital + 尤 outstanding",
           "Head straight for the <b>capital</b> (京), no detours — <b>right away</b>, <b>just</b> so: 马上就到."),
    "出": ("山 mountain stacked on 山 mountain",
           "Climbing <b>mountain</b> (山) after mountain to get <b>out</b> of the valley."),
    "会": ("人 person + 云 cloud",
           "<b>People</b> (人) gathering like <b>clouds</b> (云) — a meeting; skills you've gathered are what you <b>can</b> do."),
    "可": ("丁 nail + 口 mouth",
           "The <b>mouth</b> (口) nails it down (丁): “<b>Can</b> do — approved!”"),
    "也": ("pictograph: a wriggling snake",
           "Whatever that wriggly thing is — I want one <b>too</b>! 也 = also."),
    "你": ("亻 person + 尔 (≈ <b>ní</b>, sound)",
           "A <b>person</b> (亻) standing right in front of you — 你 is “<b>you</b>.”"),
    "对": ("又 hand + 寸 inch",
           "A <b>hand</b> (又) measuring to the exact <b>inch</b> (寸) — precisely <b>correct</b>: 对!"),
    "生": ("pictograph: a sprout on the earth",
           "A sprout pushing out of the soil — <b>life</b>, being <b>born</b>: 生日 birth-day."),
    "能": ("pictograph: a bear (月 body + 匕匕 claws)",
           "Originally a <b>bear</b> — strong and capable: <b>can</b>, able to."),
    "而": ("pictograph: a beard",
           "A wise man strokes his <b>beard</b> between clauses: “…<b>and</b> moreover…” (而且)."),
    "子": ("pictograph: a baby with open arms",
           "A swaddled <b>baby</b> with arms out — child; also the little suffix on nouns: 儿子, 桌子."),
    "那": ("刀-shape + 阝 city",
           "Pointing at the <b>city</b> (阝) on the horizon — <b>that</b> one over there."),
    "得": ("彳 step + 旦 dawn + 寸 inch",
           "<b>Step</b> (彳) out at <b>dawn</b> (旦) and come home with treasure in <b>hand</b> (寸) — to <b>get</b>: 得到."),
    "于": ("a simple signpost",
           "A little signpost pinning a phrase to its place — <b>at/in</b> (formal): 取决于 depends <b>on</b>."),
    "着": ("羊 sheep + 目 eye",
           "Your <b>eye</b> (目) fixed on the <b>sheep</b> (羊), continuously — the “-ing” particle: 等着 waiting."),
    "下": ("a mark below the base line",
           "A root growing <b>below</b> the ground line — <b>down</b>, under: 下雨 rain comes down."),
    "自": ("pictograph: a nose",
           "A <b>nose</b> — Chinese speakers point at their nose to say “me”: <b>self</b> (自己)."),
    "之": ("pictograph: a footprint",
           "A classical <b>footstep</b> linking one thing to the next — the old-style 的: 总之 in a word."),
    "年": ("a person carrying grain",
           "A farmer carrying home the <b>harvest</b> — one harvest = one <b>year</b>."),
    "过": ("辶 walk + 寸 inch",
           "<b>Walking</b> (辶) past, inch (寸) by inch — to <b>cross</b>; once crossed it's experience: 去过 have been."),
    "发": ("又 hand + strokes flying off",
           "A <b>hand</b> (又) letting the arrow fly — to <b>send out</b>: 发消息 send a message."),
    "后": ("厂 slope + 口 mouth",
           "The voice (口) calling from <b>behind</b> the hill (厂) — <b>after</b>, behind: 以后 afterwards."),
    "作": ("亻 person + 乍 (≈ <b>zhà</b>, sound)",
           "A <b>person</b> (亻) springing suddenly (乍) into action — to <b>do</b>, to make: 工作 work."),
    "里": ("田 field + 土 earth",
           "<b>Fields</b> (田) on <b>earth</b> (土) — the village you live <b>in</b>: inside."),
    "用": ("pictograph: a bucket",
           "An old wooden bucket — grab it and put it to <b>use</b>."),
    "道": ("辶 walk + 首 head",
           "Your <b>head</b> (首) leading your feet (辶) down the <b>path</b> — the way; 知道 = know the way."),
    "行": ("pictograph: a crossroads",
           "A <b>crossroads</b> where everything moves — to go; “行!” = it works, OK, let's go."),
    "所": ("户 door + 斤 axe",
           "The <b>door</b> (户) where the <b>axe</b> (斤) hangs — the <b>place</b>; 所以: from that place follows… <b>so</b>."),
    "然": ("月 meat + 犬 dog + 灬 fire",
           "Meat roasting over the <b>fire</b> (灬) — “<b>so</b> it is, naturally”; 然后 = after that."),
    "家": ("宀 roof + 豕 pig",
           "A <b>pig</b> (豕) under your <b>roof</b> (宀) — wealth and warmth: <b>home</b>."),
    "种": ("禾 grain + 中 (≈ <b>zhōng</b>, sound)",
           "Each <b>grain</b> (禾) is its own <b>kind</b> of seed — 这种 this kind; plant it and it's 种 zhòng, to sow."),
    "事": ("a hand holding a writing tablet",
           "A hand holding the official record — the <b>matter</b> at hand; 没事 = nothing on record, no worries."),
    "成": ("戈 weapon, forged complete",
           "The <b>weapon</b> (戈) comes off the forge finished — to <b>become</b>, to succeed: 成功."),
    "方": ("pictograph: a square with corners",
           "A <b>square</b> pointing all four directions — 地方: a place on the map."),
    "多": ("夕 evening + 夕 evening",
           "<b>Evening</b> (夕) after evening piling up — <b>many</b>, much."),
    "经": ("纟 silk + loom warp",
           "The <b>silk</b> (纟) warp threads of a loom — everything <b>passes through</b> them: 已经 already passed."),
    "么": ("a tiny silk thread",
           "A tiny dangling <b>thread</b> tacked onto question words: 什么? 怎么?"),
    "去": ("土 earth + 厶 private",
           "Pack your <b>private</b> (厶) things and leave this patch of <b>earth</b> (土) — to <b>go</b>."),
    "法": ("氵 water + 去 go",
           "<b>Law</b> should flow like <b>water</b> (氵) — level and fair; 办法: the way water finds through."),
    "学": ("hands pouring in + 子 child under a roof",
           "Hands pouring knowledge over a <b>child</b> (子) under the school roof — to <b>learn</b>."),
    "如": ("女 woman + 口 mouth",
           "Do <b>as</b> the wise woman (女) says (口) — like, as, <b>if</b>: 如果."),
    "都": ("者 + 阝 city",
           "The capital <b>city</b> (阝) where everyone gathers — <b>all</b>: 我们都."),
    "同": ("一 one + 口 mouth under one frame",
           "Everyone under one roof-frame speaking with <b>one mouth</b> (口) — the <b>same</b>: 同学 study-mates."),
    "现": ("王 jade + 见 see",
           "<b>Jade</b> (王) suddenly <b>seen</b> (见) sparkling — it appears right <b>now</b>: 现在."),
    "当": ("彐 broom-hand over a base",
           "Pick up the broom and <b>serve as</b> the one in charge — 当医生 work as a doctor; 当然 of course, as one should."),
    "没": ("氵 water + 殳 tool",
           "It sank into the <b>water</b> (氵) — gone: <b>not have</b>, didn't happen: 没有."),
    "动": ("云 cloud + 力 force",
           "<b>Force</b> (力) pushing the <b>clouds</b> (云) across the sky — things <b>move</b>: 别动! don't move!"),
    "面": ("pictograph: a face in a frame",
           "A <b>face</b> — face, surface, side; 见面: see each other's faces, meet. (Also noodles: 面条.)"),
    "起": ("走 walk + 己 self (≈ <b>jǐ</b>, sound)",
           "Get your<b>self</b> (己) up and <b>walking</b> (走) — to <b>rise</b>; 一起 = rise as one, together."),
    "看": ("手 hand + 目 eye",
           "A <b>hand</b> (手) shading the <b>eyes</b> (目) to peer into the distance — to <b>look</b>."),
    "定": ("宀 roof + a straightened base",
           "Everything set straight under the <b>roof</b> (宀) — settled, <b>fixed</b>: 一定 for sure."),
    "天": ("大 big person + 一 line above",
           "The line <b>above</b> even the biggest person (大) — the <b>sky</b>; each sky-cycle is a <b>day</b>."),
    "分": ("八 split + 刀 knife",
           "A <b>knife</b> (刀) cutting a thing in <b>two</b> (八) — to divide; a <b>minute</b> is a slice of the hour."),
    "还": ("辶 walk + 不 no",
           "<b>Walking</b> (辶) back, “<b>not</b> (不) done yet” — <b>still</b>, also: 还好 still OK."),
    "进": ("辶 walk + 井 well",
           "<b>Walk</b> (辶) toward the <b>well</b> (井) at the village center — to <b>enter</b>: 请进!"),
    "好": ("女 woman + 子 child",
           "A <b>woman</b> (女) holding her <b>child</b> (子) — mother and child together: everything is <b>good</b>."),
    "小": ("a hook splitting two tiny dots",
           "One stroke splitting a thing into two tiny bits — <b>small</b>."),
    "部": ("咅 (≈ sound) + 阝 city",
           "A <b>city</b> (阝) split into districts — a <b>part</b>, a section; also the measure word for films."),
    "其": ("pictograph: a winnowing basket",
           "Pointing at the basket: “<b>its</b> contents, <b>that</b> one” — 其实: as for the actual facts…"),
    "些": ("此 this + 二 two",
           "<b>This</b> (此) plus another one or <b>two</b> (二) — <b>some</b>, a few."),
    "主": ("丶 flame + 王 king",
           "The <b>flame</b> (丶) burning above the king (王) — the <b>master</b> of the house keeps the lamp; 主意: the master's <b>idea</b>."),
    "样": ("木 wood + 羊 sheep (≈ <b>yáng</b>, sound)",
           "A <b>wooden</b> (木) mold shaped like a <b>sheep</b> (羊) — a pattern, a <b>way</b>: 怎么样? what's it like?"),
    "理": ("王 jade + 里 (≈ <b>lǐ</b>, sound)",
           "Carving <b>jade</b> (王) along its natural veins — following the inner <b>logic</b>: 道理 sense."),
    "心": ("pictograph: a heart",
           "A <b>heart</b> with three drops of blood — heart, mind; 担心 = a weighed-down heart: worry."),
    "她": ("女 woman + 也 also",
           "Just like 他, but with <b>woman</b> (女) — <b>she</b>."),
    "本": ("木 tree + 一 mark at the root",
           "A stroke marking the tree's (木) <b>root</b> — the origin: 本来 = from the root, originally."),
    "前": ("the prow of a boat cutting ahead",
           "A boat's prow cutting <b>forward</b> — in front, <b>before</b>: 以前 = the stretch already sailed."),
    "开": ("一 bar + 廾 two hands",
           "Two hands (廾) lifting the <b>bar</b> (一) off the door — to <b>open</b>; 开车: open the throttle, drive."),
    "但": ("亻 person + 旦 dawn (≈ <b>dàn</b>, sound)",
           "At <b>dawn</b> (旦) the person (亻) adds: “<b>but</b> one more thing…” — 但是."),
    "因": ("大 person inside 囗 box",
           "The <b>person</b> (大) found inside the sealed box (囗) — there's your <b>cause</b>: 因为 because."),
    "只": ("口 mouth + 八 a puff of breath",
           "One mouth (口), one puff (八) — <b>only</b> that, <b>just</b>: 只是."),
    "从": ("人 person + 人 person",
           "One person walking behind another — to follow; where you follow <b>from</b>: 你从哪儿来?"),
    "想": ("木 tree + 目 eye + 心 heart",
           "Your <b>eye</b> (目) on a distant tree (木), your <b>heart</b> (心) underneath — to <b>think</b> of, to <b>miss</b>."),
    "实": ("宀 roof + 头 head",
           "Real goods piled head-high (头) under the <b>roof</b> (宀) — solid, <b>real</b>: 其实 actually."),
    "日": ("pictograph: the sun",
           "A round <b>sun</b> squared off, spot in the middle — sun, <b>day</b>."),
    "军": ("冖 cover + 车 chariot",
           "<b>Chariots</b> (车) under camouflage <b>cover</b> (冖) — the <b>army</b>."),
    "者": ("耂 old master + 日",
           "The old master (耂) who does the thing — the do-<b>er</b>; 或者: this one <b>or</b> that one."),
    "意": ("音 sound + 心 heart",
           "The <b>sound</b> (音) your <b>heart</b> (心) makes — <b>meaning</b>, idea: 意思."),
    "无": ("a dancer's sweeping arms",
           "A dancer's arms sweeping over emptiness — <b>nothing</b>, without: 无所谓 nothing worth mentioning, whatever."),
    "力": ("pictograph: a flexed arm at the plow",
           "A flexed <b>arm</b> straining at the plow — <b>strength</b>: 努力 push your strength."),
    "它": ("宀 roof + 匕 (originally a snake)",
           "The thing under the <b>roof</b> (宀) that isn't a person — originally a snake: <b>it</b>."),
    "与": ("from 與: hands passing something over",
           "Hands passing something across — together <b>with</b>, and: 与我无关 nothing passed to me."),
    "长": ("pictograph: long flowing hair",
           "<b>Long</b> hair streaming — cháng = long; hair keeps <b>growing</b> — zhǎng = grow."),
    "把": ("扌 hand + 巴 (≈ <b>bā</b>, sound)",
           "A <b>hand</b> (扌) gripping the handle — 把 grabs the object and moves it up front: 把门关上."),
    "机": ("木 wood + 几 (≈ <b>jī</b>, sound)",
           "A <b>wooden</b> (木) contraption on a small table (几) — a <b>machine</b>: 手机 = hand-machine, phone."),
    "十": ("a full cross",
           "A complete cross — both directions covered: <b>ten</b>, completeness."),
    "民": ("pictograph: an old branding mark",
           "The masses marked for service in ancient times — the <b>people</b>: 移民 moving people, immigrants."),
    "第": ("⺮ bamboo + 弟 (≈ <b>dì</b>, sound)",
           "<b>Bamboo</b> (⺮) slips numbered in sequence — the ordinal prefix: 第一 first."),
    "公": ("八 divide + 厶 private",
           "<b>Dividing</b> (八) up what was <b>private</b> (厶) — <b>public</b>: 公司 the public office, a company."),
    "此": ("止 stop + 匕",
           "Your foot <b>stops</b> (止) right here — <b>this</b> (formal): 因此 therefore."),
    "已": ("a snake already coiled",
           "The snake has <b>already</b> coiled up for the day — done: 已经."),
    "工": ("pictograph: a carpenter's square",
           "A carpenter's square — <b>work</b>, labor: 工作."),
    "使": ("亻 person + 吏 official",
           "The <b>official</b> (吏) standing over a <b>person</b> (亻), making them act — to make, to <b>cause</b>; 即使 even if."),
    "情": ("忄 heart + 青 (≈ <b>qīng</b>, sound)",
           "What the young green (青) <b>heart</b> (忄) is full of — <b>feelings</b>: 心情 mood."),
    "明": ("日 sun + 月 moon",
           "<b>Sun</b> (日) and <b>moon</b> (月) side by side — the brightest things there are: <b>bright</b>, clear: 明白."),
    "性": ("忄 heart + 生 born (≈ <b>shēng</b>, sound)",
           "The <b>heart</b> (忄) you're <b>born</b> (生) with — your <b>nature</b>: 性格 character."),
    "知": ("矢 arrow + 口 mouth",
           "When you <b>know</b>, your words (口) fly straight as <b>arrows</b> (矢) — 知道."),
    "全": ("人 person + 王 jade",
           "A flawless piece of <b>jade</b> (王) held safe in a person's hands — <b>whole</b>, complete: 全都 all of it."),
    "三": ("three strokes",
           "Three strokes — <b>three</b>."),
    "又": ("pictograph: a right hand",
           "The same right <b>hand</b> reaching out yet <b>again</b> — 又迟到了! late again!"),
    "关": ("from 關: a bar across a gate",
           "The bar dropped across the gate — to <b>close</b>; 没关系: no gate between us, no worries."),
    "点": ("占 (≈ <b>zhàn</b>, sound) + 灬 fire",
           "Tiny <b>flames</b> (灬) — <b>dots</b>; the clock's dots give 几点 what o'clock; a dot of something = 一点儿 a bit."),
    "正": ("一 line + 止 foot stopping",
           "Your foot (止) <b>stops</b> exactly at the line (一) — <b>exactly</b> right: 正在 right in the middle of doing."),
    "业": ("pictograph: a rack of ceremonial bells",
           "The instrument rack you practice at daily — your <b>trade</b>: 作业 homework, 毕业 graduate."),
    "外": ("夕 evening + 卜 divination",
           "Reading omens (卜) in the <b>evening</b> (夕) was done <b>outside</b> the house — 外面."),
    "将": ("爿 side + 月 + 寸 hand",
           "The general (将军!) plans what his hand (寸) <b>will</b> do next — 将来 the future."),
    "两": ("one frame + two figures inside",
           "<b>Two</b> people riding in one cart frame — 两个人; the measure-word twin of 二."),
    "高": ("pictograph: a tall watchtower",
           "A watchtower with roof upon roof — <b>tall</b>, high."),
    "间": ("门 door + 日 sun",
           "<b>Sunlight</b> (日) slipping through the crack of the <b>door</b> (门) — the space <b>between</b>: 房间 a room."),
    "由": ("田 field + a sprout poking out",
           "A sprout pushing up <b>from</b> the field (田) — from, <b>due to</b>: 由于."),
    "问": ("门 door (≈ <b>mén</b>, sound) + 口 mouth",
           "Put your <b>mouth</b> (口) up to the <b>door</b> (门) and <b>ask</b> a question."),
    "很": ("彳 step + 艮 stubborn",
           "A <b>stubborn</b> (艮) determined stride (彳) — emphatically so: <b>very</b>."),
    "最": ("日 sun + 取 take",
           "<b>Take</b> (取) the top spot under the <b>sun</b> (日) — the <b>most</b>: 最喜欢 favorite."),
    "重": ("a figure carrying a loaded sack",
           "Bent under a loaded sack — <b>heavy</b>; and what's heavy, matters: 重要 important."),
}


# ===================== LESSONS: mini usage lessons =====================
# The hardest characters in the top-500 are the little grammar workhorses:
# one gloss + one sentence can't cover 的 or 可. Each lesson is a list of
# distinct uses -- an optional pattern head ("h", with pinyin generated
# unless "py" is given), a one-line gloss ("g"), and usually a tiny example
# ("ex" + "en"). Uses without an example render as a plain note line.

LESSONS = {
    "的": {
        "intro": "The single most common character in Chinese — a little glue word with three main jobs.",
        "uses": [
            {"h": "我的", "g": "possession: X 的 Y = “X's Y”",
             "ex": "这是妈妈的钥匙。", "en": "These are mom's keys."},
            {"h": "红色的", "g": "glues any description to a noun: description + 的 + noun",
             "ex": "我喜欢红色的车。", "en": "I like red cars."},
            {"h": "吃的", "g": "drop the noun and 的 means “the … one / … stuff”",
             "ex": "有什么好吃的？", "en": "Anything tasty around?"},
            {"h": "是……的", "py": "shì … de",
             "g": "wraps up how/when/where something happened, for emphasis",
             "ex": "我是坐飞机来的。", "en": "I came by plane (that's how)."},
            {"g": "Watch the readings: dì in 目的 (mùdì, goal), dí in 的确 (díquè, indeed)."},
        ],
    },
    "一": {
        "intro": "Not just the number one — 一 powers a bunch of everyday patterns.",
        "uses": [
            {"h": "一个", "g": "plain counting: 一 + measure word + noun",
             "ex": "我买了一个包子。", "en": "I bought a steamed bun."},
            {"h": "一下", "g": "verb + 一下 softens it: “real quick, a bit”",
             "ex": "你等一下！", "en": "Wait a sec!"},
            {"h": "一点儿", "g": "“a little”",
             "ex": "我会说一点儿中文。", "en": "I speak a little Chinese."},
            {"h": "一起", "g": "“together”",
             "ex": "明天一起吃饭吧。", "en": "Let's eat together tomorrow."},
        ],
    },
    "是": {
        "uses": [
            {"h": "A 是 B", "py": "A shì B", "g": "identifying things: “A is B”",
             "ex": "他是我哥哥。", "en": "He's my older brother."},
            {"h": "是啊", "g": "agreeing: “yeah, right?”",
             "ex": "是啊，太贵了！", "en": "Yeah, way too expensive!"},
            {"h": "是……的", "py": "shì … de",
             "g": "emphasizes when/how/where something happened",
             "ex": "这是我自己做的。", "en": "I made this myself."},
        ],
    },
    "不": {
        "uses": [
            {"g": "straight negation, right before a verb or adjective",
             "ex": "我今天不去。", "en": "I'm not going today."},
            {"h": "去不去", "g": "verb + 不 + verb makes a casual yes/no question",
             "ex": "你去不去？", "en": "Are you going or not?"},
            {"h": "不用", "g": "“no need” — the polite way to decline",
             "ex": "不用了，谢谢！", "en": "No need, thanks!"},
        ],
    },
    "了": {
        "intro": "The famous 了 — two core jobs cover most of what you'll meet.",
        "uses": [
            {"g": "right after the verb: the action happened, it's done",
             "ex": "我买了三本书。", "en": "I bought three books."},
            {"g": "at the end of the sentence: new situation — something changed",
             "ex": "下雨了！", "en": "It's raining (now)!"},
            {"h": "太……了", "py": "tài … le", "g": "“too / so …!” — exclamation frame",
             "ex": "太好了！", "en": "Awesome!"},
            {"h": "别……了", "py": "bié … le", "g": "“stop …ing”",
             "ex": "别哭了。", "en": "Stop crying."},
        ],
    },
    "在": {
        "uses": [
            {"g": "full verb: to be at / in",
             "ex": "他在办公室。", "en": "He's in the office."},
            {"g": "before a place: “in/at …” + action",
             "ex": "我在北京工作。", "en": "I work in Beijing."},
            {"h": "在 + verb", "py": "zài + verb", "g": "happening right now: “-ing”",
             "ex": "我在做饭。", "en": "I'm cooking."},
        ],
    },
    "个": {
        "uses": [
            {"g": "the default measure word: number + 个 + noun",
             "ex": "我们班有二十个学生。", "en": "There are twenty students in our class."},
            {"g": "not sure which measure word fits? 个 is usually a safe fallback",
             "ex": "我有一个问题。", "en": "I have a question."},
            {"h": "这个 / 那个", "py": "zhè ge / nà ge",
             "g": "“this one / that one” — also the classic um-uh filler while you think",
             "ex": "我要这个，不要那个。", "en": "I want this one, not that one."},
        ],
    },
    "上": {
        "uses": [
            {"h": "上班 / 上课", "py": "shàng bān / shàng kè",
             "g": "to go to (work, class) — 上 as “attend”",
             "ex": "我八点上班。", "en": "I start work at eight."},
            {"h": "上车 / 上网", "py": "shàng chē / shàng wǎng",
             "g": "to get on / go online — 上 as “get onto”",
             "ex": "快上车！", "en": "Get in the car, quick!"},
            {"h": "上个", "g": "“last” with time words",
             "ex": "上个星期我很忙。", "en": "I was really busy last week."},
        ],
    },
    "为": {
        "uses": [
            {"h": "为你", "g": "“for” someone — for their sake",
             "ex": "我为你骄傲！", "en": "I'm proud of you!"},
            {"h": "为什么", "g": "“why” — literally “for what”",
             "ex": "你为什么不吃？", "en": "Why aren't you eating?"},
            {"h": "为了", "g": "“in order to” — states the purpose up front",
             "ex": "为了健康，我每天跑步。", "en": "I run every day for my health."},
            {"h": "认为", "g": "read wéi (2nd tone) in 认为 “to think” and 成为 “to become”",
             "ex": "我认为你是对的。", "en": "I think you're right."},
        ],
    },
    "地": {
        "uses": [
            {"g": "de: adjective + 地 + verb turns it into an adverb (“-ly”)",
             "ex": "她开心地笑了。", "en": "She smiled happily."},
            {"g": "dì: the ground, the earth",
             "ex": "地上都是水。", "en": "There's water all over the floor."},
            {"g": "dì also starts place words: 地图 map, 地铁 metro",
             "ex": "我们坐地铁去吧。", "en": "Let's take the metro."},
        ],
    },
    "以": {
        "uses": [
            {"h": "可以", "g": "“can, may” — by far the most common 以",
             "ex": "这里可以拍照吗？", "en": "Can I take photos here?"},
            {"h": "以为", "g": "“to think (wrongly)” — for assumptions that turned out false",
             "ex": "我以为今天是周五。", "en": "I thought today was Friday."},
            {"h": "以前 / 以后", "py": "yǐ qián / yǐ hòu", "g": "“before / after”",
             "ex": "以后再说吧。", "en": "Let's talk about it later."},
            {"h": "所以", "g": "“so, therefore”",
             "ex": "太晚了，所以我先走了。", "en": "It was too late, so I left."},
        ],
    },
    "要": {
        "uses": [
            {"g": "“to want” something",
             "ex": "我要一份炒饭。", "en": "I'll have a fried rice."},
            {"g": "about to / going to — near future",
             "ex": "电影要开始了。", "en": "The movie's about to start."},
            {"g": "“must, need to” — advice and obligations",
             "ex": "开车要小心。", "en": "Be careful driving."},
            {"h": "要是", "g": "“if” — colloquial cousin of 如果",
             "ex": "要是下雨，我们就不去了。", "en": "If it rains, we won't go."},
        ],
    },
    "就": {
        "intro": "Tiny but everywhere — 就 flavors sentences more than it translates.",
        "uses": [
            {"g": "“right away, as soon as” — sooner than expected",
             "ex": "我马上就好。", "en": "I'll be ready in a sec."},
            {"g": "“then” — links a condition to its result",
             "ex": "你累了就休息吧。", "en": "If you're tired, then rest."},
            {"g": "“only, just”",
             "ex": "我就看了一眼。", "en": "I only took one look."},
            {"g": "“exactly, precisely” — emphasis",
             "ex": "这就是我想要的！", "en": "This is exactly what I wanted!"},
        ],
    },
    "会": {
        "uses": [
            {"g": "a learned skill: “know how to”",
             "ex": "我会游泳。", "en": "I can swim."},
            {"g": "“will (probably)” — predictions",
             "ex": "明天会下雨。", "en": "It'll rain tomorrow."},
            {"h": "一会儿", "g": "“a little while”",
             "ex": "等一会儿再走吧。", "en": "Let's leave in a little while."},
            {"h": "开会", "g": "a meeting — the noun side of 会",
             "ex": "我在开会，晚点儿说。", "en": "I'm in a meeting — talk later."},
        ],
    },
    "可": {
        "intro": "Little 可 does three very different jobs — this is one to slow down on.",
        "uses": [
            {"h": "可以", "g": "“can, may” — permission and possibility",
             "ex": "我可以坐这儿吗？", "en": "May I sit here?"},
            {"h": "可是", "g": "“but, however” — softer than 但是, everywhere in speech",
             "ex": "我很想去，可是没时间。", "en": "I really want to go, but I don't have time."},
            {"g": "可 alone, for emphasis: “really, definitely”",
             "ex": "这可不行！", "en": "That really won't do!"},
            {"h": "可能", "g": "“maybe, possibly” — 可 as “-able” also gives 可爱 cute, 可怕 scary",
             "ex": "他可能忘了。", "en": "He probably forgot."},
        ],
    },
    "对": {
        "uses": [
            {"g": "“correct, right”",
             "ex": "对，就是这样！", "en": "Right, exactly!"},
            {"h": "对我", "g": "“to, towards” someone",
             "ex": "她对我很好。", "en": "She treats me really well."},
            {"h": "对不起", "g": "“sorry”",
             "ex": "对不起，我来晚了。", "en": "Sorry I'm late."},
            {"h": "对了", "g": "“oh right, by the way” — topic switcher",
             "ex": "对了，你周末有空吗？", "en": "Oh right — are you free this weekend?"},
        ],
    },
    "能": {
        "uses": [
            {"g": "can — the circumstances allow it",
             "ex": "我今晚不能来。", "en": "I can't come tonight."},
            {"g": "polite requests",
             "ex": "你能帮我一下吗？", "en": "Could you help me out?"},
            {"g": "能 vs 会 vs 可以: 会 = learned skill, 能 = circumstances allow, 可以 = permission."},
        ],
    },
    "得": {
        "intro": "Three readings, three jobs — the classic triple-duty character.",
        "uses": [
            {"h": "得到", "g": "dé — to get, to obtain",
             "ex": "他得到了大家的信任。", "en": "He earned everyone's trust."},
            {"h": "跑得快", "g": "de — verb + 得 + how well: manner or degree",
             "ex": "你跑得真快！", "en": "You run so fast!"},
            {"h": "得走了", "py": "děi zǒu le", "g": "děi — “must, gotta” (colloquial)",
             "ex": "我得走了。", "en": "I've gotta go."},
        ],
    },
    "着": {
        "uses": [
            {"g": "zhe — an ongoing state: something stays that way",
             "ex": "别站着，坐吧。", "en": "Don't just stand there — sit."},
            {"h": "V着V", "py": "V zhe V", "g": "zhe — doing one thing while doing another",
             "ex": "我们走着去吧。", "en": "Let's walk there."},
            {"h": "睡不着", "g": "zháo — after a verb: managed to (睡着 = fall asleep)",
             "ex": "我昨晚睡不着。", "en": "I couldn't fall asleep last night."},
            {"h": "着急", "g": "zháo also in 着急 “worried, anxious”",
             "ex": "别着急，慢慢来。", "en": "No rush — take your time."},
        ],
    },
    "下": {
        "uses": [
            {"h": "下个", "g": "“next” with time words",
             "ex": "下个月我去中国。", "en": "Next month I'm going to China."},
            {"h": "下车", "g": "to get off / go down",
             "ex": "我们下一站下车。", "en": "We get off at the next stop."},
            {"h": "一下", "g": "verb + 一下: “real quick”",
             "ex": "我看一下。", "en": "Let me take a quick look."},
        ],
    },
    "过": {
        "uses": [
            {"g": "to cross, to pass",
             "ex": "过马路要小心。", "en": "Be careful crossing the street."},
            {"h": "吃过", "g": "verb + 过: “have ever done” — life experience",
             "ex": "你吃过北京烤鸭吗？", "en": "Have you ever had Peking duck?"},
            {"g": "with time: after, in (some days)",
             "ex": "过两天再说。", "en": "Let's revisit in a couple of days."},
        ],
    },
    "行": {
        "uses": [
            {"g": "xíng — “OK, that works”",
             "ex": "行，就这么定了。", "en": "OK, it's settled."},
            {"h": "自行车", "g": "xíng — to go, travel: the “self-go vehicle”, a bike",
             "ex": "我骑自行车上班。", "en": "I bike to work."},
            {"h": "银行", "g": "háng — row; trade; read háng in 银行 “bank”",
             "ex": "银行几点开门？", "en": "What time does the bank open?"},
        ],
    },
    "所": {
        "uses": [
            {"h": "所以", "g": "“so, therefore” — by far the most common use",
             "ex": "我没带伞，所以淋湿了。", "en": "I didn't bring an umbrella, so I got soaked."},
            {"h": "厕所", "g": "“toilet” — the other 所 you need daily",
             "ex": "请问，厕所在哪儿？", "en": "Excuse me, where's the bathroom?"},
            {"h": "所有", "g": "“all, every”",
             "ex": "所有人都到了。", "en": "Everyone's here."},
        ],
    },
    "多": {
        "uses": [
            {"g": "“many, much”",
             "ex": "今天人真多！", "en": "It's so crowded today!"},
            {"h": "多 + adj", "py": "duō + adj", "g": "asks “how …?”",
             "ex": "你多高？", "en": "How tall are you?"},
            {"h": "多少", "g": "“how many / how much”",
             "ex": "你的电话是多少？", "en": "What's your phone number?"},
            {"h": "多 + verb", "py": "duō + verb", "g": "“(do) more”",
             "ex": "多喝水，早点儿睡。", "en": "Drink more water and sleep early."},
        ],
    },
    "都": {
        "uses": [
            {"g": "“all, both” — comes after the things it covers",
             "ex": "我们都饿了。", "en": "We're all hungry."},
            {"h": "连……都", "py": "lián … dōu", "g": "“even …”",
             "ex": "他连早饭都没吃。", "en": "He didn't even eat breakfast."},
            {"h": "都……了", "py": "dōu … le", "g": "“already …!” — mild disbelief",
             "ex": "都十二点了，快睡吧！", "en": "It's already midnight — go to sleep!"},
            {"h": "首都", "g": "read dū in 首都 “capital city”",
             "ex": "北京是中国的首都。", "en": "Beijing is China's capital."},
        ],
    },
    "当": {
        "uses": [
            {"g": "dāng — to work as, to serve as",
             "ex": "她想当医生。", "en": "She wants to be a doctor."},
            {"h": "当然", "g": "“of course”",
             "ex": "当然没问题！", "en": "Of course, no problem!"},
            {"h": "上当", "g": "dàng (4th tone) — 上当 “to get fooled / scammed”",
             "ex": "我上当了！", "en": "I got scammed!"},
        ],
    },
    "还": {
        "uses": [
            {"g": "hái — “still”",
             "ex": "他还没起床。", "en": "He still hasn't gotten up."},
            {"g": "hái — “also, in addition”",
             "ex": "你还要别的吗？", "en": "Do you want anything else?"},
            {"h": "还是", "g": "“or” in questions; also “had better”",
             "ex": "你喝茶还是咖啡？", "en": "Tea or coffee?"},
            {"h": "还钱", "g": "huán — to return, to give back",
             "ex": "我下个月还钱。", "en": "I'll pay the money back next month."},
        ],
    },
    "好": {
        "uses": [
            {"g": "“good; fine” — also the all-purpose “OK”",
             "ex": "好，就这样吧。", "en": "OK, let's leave it at that."},
            {"h": "好 + adj", "py": "hǎo + adj", "g": "“so …!”",
             "ex": "今天好热啊！", "en": "It's so hot today!"},
            {"h": "好 + verb", "py": "hǎo + verb", "g": "easy / nice to …",
             "ex": "这个很好用。", "en": "This works really well."},
            {"h": "爱好", "g": "hào (4th tone) — “to be fond of”: 爱好 hobby",
             "ex": "你有什么爱好？", "en": "What are your hobbies?"},
        ],
    },
    "只": {
        "uses": [
            {"g": "zhǐ — “only, just”",
             "ex": "我只有十块钱。", "en": "I've only got ten kuai."},
            {"h": "一只", "g": "zhī — measure word for animals (and one of a pair)",
             "ex": "我家有两只猫。", "en": "I have two cats."},
        ],
    },
    "长": {
        "uses": [
            {"g": "cháng — long",
             "ex": "这条路好长啊！", "en": "This road is so long!"},
            {"g": "zhǎng — to grow (up)",
             "ex": "孩子们长大了。", "en": "The kids have grown up."},
            {"h": "长得", "g": "zhǎng — 长得 describes someone's looks",
             "ex": "你们俩长得真像！", "en": "You two look so alike!"},
        ],
    },
    "把": {
        "intro": "The famous 把-sentence: pull the object up front and say what you did to it.",
        "uses": [
            {"h": "把 X + verb", "py": "bǎ X + verb", "g": "“take X and …” — focuses on what happens to the object",
             "ex": "请把手机给我。", "en": "Please hand me the phone."},
            {"g": "great for mishaps — what got lost, broken, forgotten",
             "ex": "我把钥匙忘在家里了。", "en": "I left my keys at home."},
            {"h": "一把", "g": "measure word for things with handles",
             "ex": "带一把伞吧。", "en": "Take an umbrella."},
        ],
    },
    "又": {
        "uses": [
            {"g": "“again” — for things that already happened (future repeats use 再)",
             "ex": "我又忘带钥匙了。", "en": "I forgot my keys again."},
            {"h": "又……又", "py": "yòu … yòu", "g": "“both … and …”",
             "ex": "这家店又便宜又好吃。", "en": "This place is cheap and tasty."},
        ],
    },
    "点": {
        "uses": [
            {"g": "o'clock",
             "ex": "我们六点见。", "en": "See you at six."},
            {"h": "一点儿 / 有点儿", "py": "yì diǎnr / yǒu diǎnr",
             "g": "“a bit” — 一点儿 goes after, 有点儿 before (usually complaints)",
             "ex": "我有点儿累。", "en": "I'm a bit tired."},
            {"h": "点菜", "g": "to order food — “point at” the menu",
             "ex": "我们点菜吧！", "en": "Let's order!"},
        ],
    },
}


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
        m = MNEMONICS.get(char)
        if m:
            row["parts"], row["mnem"] = m
        else:
            print(f"WARNING: no mnemonic for #{rank} {char}")
        lesson = LESSONS.get(char)
        if lesson:
            row["lesson"] = lesson_json(char, lesson)
        out.append(row)
    return out


def lesson_json(char, lesson):
    uses = []
    for u in lesson["uses"]:
        d = {"g": u["g"]}
        h = u.get("h")
        if h:
            d["h"] = h
            if "py" in u:
                d["hPy"] = u["py"]
            elif HANZI.fullmatch(h):
                d["hPy"] = to_pinyin(h)
            else:
                raise ValueError(f"lesson head {h!r} for {char} needs explicit 'py'")
        if "ex" in u:
            d["ex"] = u["ex"]
            d["exPy"] = to_pinyin(u["ex"])
            d["exEn"] = u["en"]
        uses.append(d)
    out = {"uses": uses}
    if lesson.get("intro"):
        out["intro"] = lesson["intro"]
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
