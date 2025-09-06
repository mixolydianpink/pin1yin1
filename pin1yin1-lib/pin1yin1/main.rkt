#lang racket/base

(provide (rename-out [pin1yin1-string->pinyin pin1yin1->pinyin]
                     [pin1yin1-string->zhuyin pin1yin1->zhuyin]
                     [pin1yin1-string->pinyin/html-fragment pin1yin1->pinyin/html-fragment]
                     [pin1yin1-string->zhuyin/html-fragment pin1yin1->zhuyin/html-fragment]
                     [pin1yin1-string->pinyin/html pin1yin1->pinyin/html]
                     [pin1yin1-string->zhuyin/html pin1yin1->zhuyin/html]))

(module* test racket/base
  (require rackunit
           rackunit/text-ui)

  (require (only-in xml
                    xexpr->string))

  (require (submod "..") #|pin1yin1|#)

  (define ->pinyin/t
    (test-suite "Convert pin1yin1 to pīnyīn"

                (test-equal? "Basic"
                             (pin1yin1->pinyin "pin1yin1")
                             "pīnyīn")
                (test-equal? "Invalid pin1yin1 returns #f"
                             (pin1yin1->pinyin "A24")
                             #f)
                (test-equal? "'v' in place of 'ü'"
                             (pin1yin1->pinyin "nv3")
                             "nǚ")
                (test-equal? "Syllable separation"
                             (pin1yin1->pinyin "Chang2an1")
                             "Cháng'ān")
                (test-equal? "Hyphenated"
                             (pin1yin1->pinyin "Zhong1guo2-Mei3guo2")
                             "Zhōngguó‑Měiguó")
                (test-suite "Punctuation and whitespace"
                            (test-equal? "Whitespace"
                                         (pin1yin1->pinyin "\r \r\n#z#\tni3_hao3 ma5#s#\n")
                                         "\n \n\u200B\tnǐ hǎo ma \n")
                            (test-equal? "Punctuation"
                                         (pin1yin1->pinyin "....!?,\\:;/~()[]{}")
                                         "….!?,,:;/~()“”‘’")
                            (test-equal? "Punctuation and whitespace"
                                         (pin1yin1->pinyin "Ta1 shuo1: [Wo3_hen3 e4!]")
                                         "Tā shuō: “Wǒ hěn è!”"))
                (test-suite "Capitals"
                            (test-equal? "As word"
                                         (pin1yin1->pinyin "APP shan3tui4 le5")
                                         "APP shǎntuì le")
                            (test-equal? "Within word"
                                         (pin1yin1->pinyin "T-xu4shan1")
                                         "T‑xùshān"))
                (test-suite "Numerals"
                            (test-equal? "As word"
                                         (pin1yin1->pinyin "1990")
                                         "1990")
                            (test-equal? "Within word"
                                         (pin1yin1->pinyin "di4-12")
                                         "dì‑12"))
                (test-equal? "Erhua"
                             (pin1yin1->pinyin "nar4_dian(r)3")
                             "nàr diǎn(r)")
                (test-equal? "Implicit word-final neutral tone"
                             (pin1yin1->pinyin #:implicit-neutral-tone? #t
                                               "xian1sheng")
                             "xiānsheng")
                (test-equal? "Explicit neutral tone"
                             (pin1yin1->pinyin #:explicit-neutral-tone? #t
                                               "xian1sheng5")
                             "xiān·sheng")
                (test-suite "Literal string"
                            (test-equal? "Literal string with |…|"
                                         (pin1yin1->pinyin "|Albert\r\n|_de5")
                                         "Albert\r\n de")
                            (test-equal? "Include | in literal string with ||"
                                         (pin1yin1->pinyin "|Here (||) is one vertical line|")
                                         "Here (|) is one vertical line")
                            (test-equal? "Language tag (ignored)"
                                         (pin1yin1->pinyin "#en-US|Hello|")
                                         "Hello"))
                (test-suite "Punctuation style"
                            (test-equal? "Chinese (TW)"
                                         (pin1yin1->pinyin #:punctuation 'zh-TW
                                                           "|你好| (ni3_hao3)")
                                         "你好（nǐ hǎo）")
                            (test-equal? "Chinese (TW) (with non-explicit spaces)"
                                         (pin1yin1->pinyin #:punctuation 'zh-TW+space
                                                           "Ta1 shuo1: [Ni3_hao3!]")
                                         "Tā shuō： 「Nǐ hǎo！」")
                            (test-equal? "Chinese (TW) (with non-explicit spaces as zero width)"
                                         (pin1yin1->pinyin #:punctuation 'zh-TW+space/zero-width
                                                           "Ta1 shuo1: [Ni3_hao3!]")
                                         "Tā\u200Bshuō：\u200B「Nǐ hǎo！」")
                            (test-equal? "Chinese (CN)"
                                         (pin1yin1->pinyin #:punctuation 'zh-CN
                                                           "|你好| (ni3_hao3)")
                                         "你好（nǐ hǎo）")
                            (test-equal? "Chinese (CN) (with non-explicit spaces)"
                                         (pin1yin1->pinyin #:punctuation 'zh-CN+space
                                                           "Ta1 shuo1: [Ni3_hao3!]")
                                         "Tā shuō： “Nǐ hǎo！”")
                            (test-equal? "Chinese (CN) (with non-explicit spaces as zero width)"
                                         (pin1yin1->pinyin #:punctuation 'zh-CN+space/zero-width
                                                           "Ta1 shuo1: [Ni3_hao3!]")
                                         "Tā\u200Bshuō：\u200B“Nǐ hǎo！”"))))

  (define ->zhuyin/t
    (test-suite "Convert pin1yin1 to ㄓㄨˋㄧㄣ"

                (test-equal? "Basic"
                             (pin1yin1->zhuyin "zhu4yin1")
                             "ㄓㄨˋㄧㄣ")
                (test-equal? "Invalid pin1yin1 returns #f"
                             (pin1yin1->zhuyin "A24")
                             #f)
                (test-equal? "'v' in place of 'ü'"
                             (pin1yin1->zhuyin "nv3")
                             "ㄋㄩˇ")
                (test-equal? "Hyphenated (elided)"
                             (pin1yin1->zhuyin "Zhong1guo2-Mei3guo2")
                             "ㄓㄨㄥㄍㄨㄛˊㄇㄟˇㄍㄨㄛˊ")
                (test-suite "Punctuation and whitespace"
                            (test-equal? "Whitespace"
                                         (pin1yin1->zhuyin "\r \r\n#z#\tni3_hao3 ma5#s#\n")
                                         "\n\n\u200B\tㄋㄧˇ ㄏㄠˇㄇㄚ˙\u3000\n")
                            (test-equal? "Punctuation"
                                         (pin1yin1->zhuyin "....!?,\\:;/~()[]{}")
                                         "⋯⋯。！？，、：；/～（）「」『』")
                            (test-equal? "Punctuation and whitespace"
                                         (pin1yin1->zhuyin "Ta1 shuo1: [Wo3_hen3 e4!]")
                                         "ㄊㄚㄕㄨㄛ：「ㄨㄛˇ ㄏㄣˇㄜˋ！」"))
                (test-suite "Capitals"
                            (test-equal? "As word"
                                         (pin1yin1->zhuyin "APP shan3tui4 le5")
                                         "APPㄕㄢˇㄊㄨㄟˋㄌㄜ˙")
                            (test-equal? "Within word"
                                         (pin1yin1->zhuyin "T-xu4shan1")
                                         "Tㄒㄩˋㄕㄢ"))
                (test-suite "Numerals"
                            (test-equal? "As word"
                                         (pin1yin1->zhuyin "1990")
                                         "1990")
                            (test-equal? "Within word"
                                         (pin1yin1->zhuyin "di4-12")
                                         "ㄉㄧˋ12"))
                (test-equal? "Erhua"
                             (pin1yin1->zhuyin "nar4_dian(r)3")
                             "ㄋㄚˋㄦ ㄉㄧㄢˇ（ㄦ）")
                (test-equal? "Implicit word-final neutral tone"
                             (pin1yin1->zhuyin #:implicit-neutral-tone? #t
                                               "xian1sheng")
                             "ㄒㄧㄢㄕㄥ˙")
                (test-equal? "Explicit first tone"
                             (pin1yin1->zhuyin #:explicit-first-tone? #t
                                               "xian1sheng5")
                             "ㄒㄧㄢˉㄕㄥ˙")
                (test-equal? "Prefix neutral tone mark"
                             (pin1yin1->zhuyin #:prefix-neutral-tone? #t
                                               "jue2de5")
                             "ㄐㄩㄝˊ˙ㄉㄜ")
                (test-suite "Literal string"
                            (test-equal? "Literal string with |…|"
                                         (pin1yin1->zhuyin "|Albert\r\n|_de5")
                                         "Albert\r\n ㄉㄜ˙")
                            (test-equal? "Include | in literal string with ||"
                                         (pin1yin1->zhuyin "|Here (||) is one vertical line|")
                                         "Here (|) is one vertical line")
                            (test-equal? "Language tag (ignored)"
                                         (pin1yin1->zhuyin "#en-US|Hello|")
                                         "Hello"))
                (test-suite "Punctuation style"
                            (test-equal? "Chinese (Latn)"
                                         (pin1yin1->zhuyin #:punctuation 'zh-Latn
                                                           "|你好| (ni3_hao3)")
                                         "你好 (ㄋㄧˇ ㄏㄠˇ)")
                            (test-equal? "Chinese (TW) (with non-explicit spaces)"
                                         (pin1yin1->zhuyin #:punctuation 'zh-TW+space
                                                           "Ta1 shuo1: [Ni3_hao3!]")
                                         "ㄊㄚ ㄕㄨㄛ： 「ㄋㄧˇ ㄏㄠˇ！」")
                            (test-equal? "Chinese (TW) (with non-explicit spaces as zero width)"
                                         (pin1yin1->zhuyin #:punctuation 'zh-TW+space/zero-width
                                                           "Ta1 shuo1: [Ni3_hao3!]")
                                         "ㄊㄚ\u200Bㄕㄨㄛ：\u200B「ㄋㄧˇ ㄏㄠˇ！」")
                            (test-equal? "Chinese (CN)"
                                         (pin1yin1->zhuyin #:punctuation 'zh-CN
                                                           "|你好| (ni3_hao3)")
                                         "你好（ㄋㄧˇ ㄏㄠˇ）")
                            (test-equal? "Chinese (CN) (with non-explicit spaces)"
                                         (pin1yin1->zhuyin #:punctuation 'zh-CN+space
                                                           "Ta1 shuo1: [Ni3_hao3!]")
                                         "ㄊㄚ ㄕㄨㄛ： “ㄋㄧˇ ㄏㄠˇ！”")
                            (test-equal? "Chinese (CN) (with non-explicit spaces as zero width)"
                                         (pin1yin1->zhuyin #:punctuation 'zh-CN+space/zero-width
                                                           "Ta1 shuo1: [Ni3_hao3!]")
                                         "ㄊㄚ\u200Bㄕㄨㄛ：\u200B“ㄋㄧˇ ㄏㄠˇ！”"))))

  (define ->pinyin/html/t
    (test-suite "Convert pin1yin1 to pinyin as HTML"

                (check-not-equal? (pin1yin1->pinyin/html "|Albert|_de5_T-xu4shan1.")
                                  #f)
                (check-regexp-match #rx"pīn.*yīn"
                                    (xexpr->string
                                     (pin1yin1->pinyin/html "pin1yin1")))
                (check-regexp-match #rx"class=\"first-tone\".*pīn.*yīn"
                                    (xexpr->string
                                     (pin1yin1->pinyin/html #:syllable-first-tone-class "first-tone"
                                                            "pin1yin1")))
                (check-regexp-match #rx"lang=\"zh-CN\".*<span lang=\"en-US\">Hello</span>"
                                    (xexpr->string
                                     (pin1yin1->pinyin/html #:lang "zh-CN"
                                                            "#en-US|Hello|")))
                (check-regexp-match #rx"^[^\t\r\n]*$"
                                    (xexpr->string
                                     (pin1yin1->pinyin/html "\r\t\r\n\n|\r\t\r\n\n|")))))

  (define ->zhuyin/html/t
    (test-suite "Convert pin1yin1 to zhuyin as HTML"

                (check-not-equal? (pin1yin1->zhuyin/html "|Albert|_de5_T-xu4shan1.")
                                  #f)
                (check-regexp-match #rx"ㄓㄨ.*ˋ.*ㄧㄣ"
                                    (xexpr->string
                                     (pin1yin1->zhuyin/html "zhu4yin1")))
                (check-regexp-match #rx"class=\"fourth-tone\".*ㄓㄨ.*ˋ.*ㄧㄣ"
                                    (xexpr->string
                                     (pin1yin1->zhuyin/html #:syllable-fourth-tone-class "fourth-tone"
                                                            "zhu4yin1")))
                (check-regexp-match #rx"lang=\"zh-TW\".*<span lang=\"en-US\">Hello</span>"
                                    (xexpr->string
                                     (pin1yin1->zhuyin/html #:lang "zh-TW"
                                                            "#en-US|Hello|")))
                (check-regexp-match #rx"^[^\t\r\n]*$"
                                    (xexpr->string
                                     (pin1yin1->zhuyin/html "\r\t\r\n\n|\r\t\r\n\n|")))))

  (define pin1yin1/t
    (test-suite "pin1yin1"
                ->pinyin/t
                ->zhuyin/t
                ->pinyin/html/t
                ->zhuyin/html/t))

  (run-tests pin1yin1/t))

(require pin1yin1/conversion/markup
         pin1yin1/conversion/string)
