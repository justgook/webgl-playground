module Playground.Font.SimpleMood exposing (image, letters)

--http://dwarffortresswiki.org/index.php/Character_table
--https://github.com/RogueYun/SimpleMood

import Dict


letters : Char -> Float
letters c =
    Dict.get c letters_
        |> Maybe.withDefault 0


letters_ =
    [ [ '\u{0000}', '☺', '☻', '♥', '♦', '♣', '♠', '•', '◘', '○', '◙', '♂', '♀', '♪', '♫', '☼' ]
    , [ '►', '◄', '↕', '‼', '¶', '§', '▬', '↨', '↑', '↓', '→', '←', '∟', '↔', '▲', '▼' ]
    , [ ' ', '!', '"', '#', '$', '%', '&', '\'', '(', ')', '*', '+', ',', '-', '.', '/' ]
    , [ '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', ':', ';', '<', '=', '>', '?' ]
    , [ '@', 'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O' ]
    , [ 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z', '[', '\\', ']', '^', '_' ]
    , [ '`', 'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o' ]
    , [ 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z', '{', '|', '}', '~', '⌂' ]
    , [ 'Ç', 'ü', 'é', 'â', 'ä', 'à', 'å', 'ç', 'ê', 'ë', 'è', 'ï', 'î', 'ì', 'Ä', 'Å' ]
    , [ 'É', 'æ', 'Æ', 'ô', 'ö', 'ò', 'û', 'ù', 'ÿ', 'Ö', 'Ü', '¢', '£', '¥', '₧', 'ƒ' ]
    , [ 'á', 'í', 'ó', 'ú', 'ñ', 'Ñ', 'ª', 'º', '¿', '⌐', '¬', '½', '¼', '¡', '«', '»' ]
    , [ '░', '▒', '▓', '│', '┤', '╡', '╢', '╖', '╕', '╣', '║', '╗', '╝', '╜', '╛', '┐' ]
    , [ '└', '┴', '┬', '├', '─', '┼', '╞', '╟', '╚', '╔', '╩', '╦', '╠', '═', '╬', '╧' ]
    , [ '╨', '╤', '╥', '╙', '╘', '╒', '╓', '╫', '╪', '┘', '┌', '█', '▄', '▌', '▐', '▀' ]
    , [ 'α', 'ß', 'Γ', 'π', 'Σ', 'σ', 'µ', 'τ', 'Φ', 'Θ', 'Ω', 'δ', '∞', 'φ', 'ε', '∩' ]
    , [ '≡', '±', '≥', '≤', '⌠', '⌡', '÷', '≈', '°', '∙', '·', '√', 'ⁿ', '²', '■', '\u{00A0}' ]
    ]
        |> List.concat
        |> List.indexedMap (\a b -> ( b, toFloat a ))
        |> Dict.fromList


image : String
image =
    "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAQAAAAEACAMAAABrrFhUAAAAAXNSR0IArs4c6QAAAAZQTFRFAAAA////pdmf3QAAAAJ0Uk5TAP9bkSK1AAAPR0lEQVR4nO2aiZKsOg5E7f//6RfxugEpM7UYqK1veaYvyJYl5fFSRMyM8eI2oRUOzy5J2cEkipGN62wqQwnocIq7eHIqxefj/P6hBFNBMbyzAKwl5rouLN/9sw7A9geCK7sQiO/WVwM4/j2Gj67JC/erQFd0RN/+ttR6R/AKSFOPOx2GsDFQFuFYAwAbogZgcozoSACOn+LChMUKlwAwoAbAC5QcmRLANhgdCTCHeUFeHQCH2iOGM34JVwB8PmOHB0DcAQUAaRsABOwGAL9680uQDIPPLlAJwIEEh8D/SQCgZlBxBYB94wmBHAvkyQDYigFsL1p9CwBuOZbnLsEJcS5fgiUAV90AiFy/ACALJwWQkDps/oMHjGPUFgChME6PC0l3ogJgXBSAAbOLgsg/TOfPGM7040V+P+h1/OyYoKYWANiAy0ByACvjuh6KMA69xzFqlGTFJelXgQTZHgNArnTm/9xWCmzp/7aHtxX2nfXCsfwIPbbJYqFTve994s6oCOBYZV9u7lcB3lSx0EcGPgHA7hiW4xNSAQSA/RO1OqF55xdZEH3q+WDHc/9tFZ9oUTm4pyqF0X7U8aGOdAEbBS0DUJsqlKPuDDGZCzgGyNP3c/3uy1B7UJLwDmAAQtF5ABPjYQemofyhKWoI9BcADqbDFpQAgeQ/9j5vG2e9k20EoMZd/ssAqOUAqCAG4AQjkKcA8LYgkALYitAJBABO4IGE9UZHQEYPBVf6Vy9BBHC4hzsA9Zd3gnMRczG6eYhLr9JLPYsATJgAAO2oND154Bi243IO6q30BwQgfQRArAQvqbi5KX3sUBHA2PCidxzNipg3ALBgFT3Ut6iwaLZK0bkcpeNb6K8BrBf4bd/2vIaX3c+G/VubFm9J86gBbAPhrz7M3lxCcxDeWy7JJAT/Bh5FTdfGoEvPOtphzqDzkb3hhQnnVPsk2VgbgORhJG5WmEDV4gHg/7ByBwC1LXV9HQADntPEFwDUinMF4H76SyiWmI8GAOgOoEhefwgAbK4gq4eQoHM8LiuQMbxvCgDzOWseEUxXLBd6UNAdAAr9i78CnFDQmNm2vQFA7B/ku3CEEADVvAs+3OsFyXqYLgqckD0F0NBf0bMZ7MqY+HbF8og3XII5XxUu9q3iSQC2xN8XD+DKDpjlz2BHf3A+GxPkGCk+ZlguOKGZz9s/AH/+jAPO7+pp6K83rHnYEg/Ljm/piy2AAMAhra/S5PyvfkKUAL7t277trRvdyeKSFnfyMS5sN6fhv2QP/bsvVCWClRz+lnef79EEZeOz8l+x9y7U0NSvvsydRuoYinC8CtEzi9WZvwKAJboeMj4NQBVT8iEkh+FjfAKAvMn9IJiwx1HInDGAOSGit+cMbT1dAMjiLeuvnVhOBoDrKRQ2BYXjHQATF+gEAFVIBCAK2bFV/JXxUs7JIxAWkr1nBT4SgFSYyssRSSFvDEDJId/uzyDlmqYpBxxX/ijkZn8FYAwM1jwwQTzqQvdjXOsZ4fgN/jBZiE0IyNQJr3+h/ev6v8216JL6V9qcSCCylr+E/++g8OpD0j6DS3PrmeBPnhivik8AKgHFeAlA2XcDEH7Jz64N3iD46Ce+BytLAj0pAJ7mfRCASMw9AI51bwGYDwBQCaoARPM7AA55XQAz1eNmBAFS4jIgFpQAAJcegGY8Zb8DgLDAOwAMWPFlAJFjF4AaBzssqA0gOQL0fASAVUAVsAiA9jsI9OIXAOzLXQAqINcAxDvojH8GYNo2oEOMaxsAYDwn2g/LfFV91TPSUwFABWU8BWCICRkAle8KgCQ+Xzzf9m3f9jdacrpXL7nqln7Iz6guIBLHN1p2v8lb1NjV+MsBsDgCgDzY/drz+No6LZC/RxBoKFmLs50E5F4A0Xt7frTA/PkkskUEyD0mcEX4LQBohQr9VIEiwPslJDBNiwq1FaF9CsB0/3M86s31sxRB4ByAUIBcoGBN5Hw8436+FBwL1HKZyQIAZcOKJYxzAHimGQDirK45IYXqC+VyawEwGRTRFAAIJAC0IaCmlrgGpBUAtqLfJ9oLAPwWV/EnKMD3ORColJEek0x/RMAFdh1yTbVAKucQPDCuqqnKxzn0eB+A6VcAeMsGpl+QgUZySZ0CMKJ6lN4Ooz7Dj2p9AN/2bf9Oyw5D96h0rp32vKQvuP8vHed0/mUAxZ3zNgBSAt0oWX813m0q3tUL3ROABaMFjO3w298VCVuCN4hcEOefAujXC9Fpvv7yO2OzEgnAVVgAcD74vlLfPgsSEumTT8pVzVPQ2E/H3N9n/n+IiHIQws5ENW7mPwQALN+9AEDAFQBSTBW/DWDy2G0AViZ2xxWBswDQ5SUAVp/dPE0Aof/VutwWnWTmt6q41Q9HXeAEm/e4cez4d4T7BBkB5R/b6pBKAAioBLDgXwGQl8hT23IBry74277t7dvrz8ezC4Arga2inLvLffYK4J0oAKT13F7vKwD0bSrOEYKfZerJxsnfJr/zib+DSwBoP8Ry1ZddOv4iAFISSfRyx37wD11+8uozKqA1f5r/fwAUJPOw/vKM8/IkX4arT78gNYC5P7Izpeez5EUAR4ZtbLiXrnCITiVFANS3OswXRjw+DEyVP3QA3Q6ATajsIVobQHLpqHEGjPpXfwWc7PIOCy+18wCKFf55oTRQYJi7B8CPifyhQErPK0hnTI5DYMqfPXW+SLAgltQfeW/9PM71eC89PtCILj1dIASsAUT6Q6Hd/sc3BbAx5YEVfdu/1fBM+t7BW/S4K9hX+auEV+J1/NWvciRIApDu4c+Yuq9CAqfiLfirXx1XDwZUANR7UgD5VwBW4634q3wEoJp0pVUAHt5gwwRHyPu7/k6AZ9ryMyi25/QB0C4BVAGebZ8BkD1bADK/VfvqczX3LQDcCly0VT4cz+wzANJ4FaFEz+ktHQBo2S8BoILf/eyKOwMgzRut0COfV/LdDsBUFAKJ7HnCvpJP1VvZNk4ElI1egnOXwPl8p+wOAFfPE9qz833bZ7Wf/RFu4Ucm7fvimXMRIFh+A+ifpRsByPkYLA0Od9gEmwFEd1x8B64KDBDLeNGl186GAqoVU8mivjBeIogBNOoDxbQc6AAA9n9qXNdbC7jYUNZv7g/lH29R7NjM339oQsuOhFx5ckJJcLfNQwPQgM1EM0GMl/Yjn/pGM31NAK5Z0c8W1BeOCwJdoYM2VcTPBIAdqwBArzwCdz+vAFZ7GAWZhxwHouT8UABqx60AG3797rgE6Q7Y/3kIgEWwKBD1S4FzUpcYpwVxAKIVuMPGZ+aPAsQ4OyfDBEAQ9hdF4b9qk4BqfrWCoHB7xYDjbZoQeHd4+xzA79tuaPP3Go3sT2i4K1bsn+0LR3CsbLPX78mL+t1vEdr7IY/stVvJ3GWuI7wkx7Elp3s9qZdsjMh2daufAmAvtzlwCVw8J1oTeGQ7CtT2YMBFvG1KEwBIJgK0InKD7l02cTQ+cbwAsNJQrwBAdEkwAJF7lPWvfLmtASAaFA98Yb3MQwHDUdEFRaj3ledWIxIE28tl/fvKIiDqYNEeD883nVjgPQBMvIOIlzekucc6AOAO2eJl+sMTT51c8N0A8hatcGzvPPcFNA8EUB8BFHAHAHdwEYgwf196AH6nbltgM71mL4D42I6HALA1NAFE+dWYXfISgBDsgEzbggL1cArC15A0BaDpb4EkAOjIO/2ZQhyXdgGiJcgnKCe4vOIOUDto+5f1v7wt6v+229t0f/Sf6f9+pvAWSy8t5ROWs3ikOv5Zbi////+6P0DQAjBV8iaA2RG06J+fLyTg/+QGSADwgc4FoPth6Oe0bcm/J788Aw8D4OY3AcwbACzq/4MA7rgDSIAZpwIbACS4BID1h4EawMU7QCZIVhTqJ38fRD2jORqQrsfmpr+1O0ATbgKgY7IAILCr+KreS3fA5wO4eAdIABPHiwIJ4AkAU8QP63Fzr90BZQIAUF1Sx6sGgOMSKJaWLNALvgOEIAJY+V8EAPVfugM+H8DFO+CtALD+ocdcxw13QHInVADIvwDA42eazUd/1+4AthsAlvxvBtATVAYoxleBqLl3jr8EAB4f9YxusWT8zPxJvk8B8Js1BTCds5kluyle1LL5bwdgVcAXwF0AJtZbCEzGaX5YsIUVA8AgEkBCIJ1fzF2IVfnD7tjGzVAHrqS94B4HeTQALDgDsFr7NfUnBf0pAH1Fi+7NI4D3xWrpV+V3cvCltuoPK775ILB9apPqmr8eahHI7I6/AsB1rf6s3fQz+AXwQgADCvq7AGAsiuEK1sPyQ6rrf6rdAQDfOwA641fj1Q4N/xW4nflxvGoLV/1i/l0AVo+TmUI1vxBAlSABYP3b83W+jwfQn/8F8HIA8OwBMPNWAey9Sb4g80MAdAMe48ofpzpbpR1AUORr1f9OAHBWlnoVQKj/CyAU1gRzAsAQ/jjV2lF5Rb60ihsBlIK3eel8CcROhMv2gFQVjkCfD6D1pVgBsPP9sw9A1/cF8B4A8Oh+MADjDcn70/kSzJ6YL4rnhTwWAP2MYd+V+SpeFfvpAL7tn26wj7KN1RlfzbdsX80Xj9svtucBqPJrmz5GG+k80Z+X0bi0IIJ9RuPBfFcUvjef07WxySB9Lr2sx0asElNEwx4LiufDlEUAzt18cOxagnoVgIETENBwbXew9cvVjApnDcdoKVzp8QB2uxvPiVgH4AlsFUWCJADT1VkxM94GEC5IACDaci8BYA81PEsAdnc/CICLfw7ATPwh4PsB8PolAC8RAQw3KPJukk4BwPo1AFMgFlwBmEdi4y8BQAQuShU4j4DREXAZovqj57EBOQCtTghAAmRxEgBMkPEAAI/LAGE8X9c+dgCAxSEA/hFkVGIhhCpYCsAzKqrqxmcAPDmJ/CHtD0j47Kb2c7VrP7uBvALAm4FQy8P3oJgjQhgb3+iHjWZfVBDbrekJACagJNiFdQCc27C7Rf9urtTMhrC7wWIAcsFxvAvAHgj55bRSsgtLBYH3cnjskgbnGwAAFMMukACqcisAYr2SiLQD7gXg+0UMBsD1pvkIgCp+BYBOWB8BZYc6mHcCQMRHi6b374QagBRcFJgAUPvHRcgA0HKHAOL4jXbiZxCHRSBz/ovs8RH4f2RNzZkdsNgyAL7xr2Annih3sboVAKv2kFs0+hkEHCfaxwMYZ1aRCj4590HNA0Bakdcd6d6koUzBAxb97SRca/GSRADepf0HLcFaUmZNR2YAAAAASUVORK5CYII="
