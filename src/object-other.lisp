;;;; src/object-other.lisp
;;;;
;;;; Recognition for Org object forms outside links and delimited markup.
(in-package #:org-kit)

;; Org 9.8.10's entity table is embedded so parsing performs no I/O.
(defparameter *org-entity-table*
  '(("Agrave" "\\`{A}" nil "&Agrave;" "A" "À" "À")
    ("agrave" "\\`{a}" nil "&agrave;" "a" "à" "à")
    ("Aacute" "\\'{A}" nil "&Aacute;" "A" "Á" "Á")
    ("aacute" "\\'{a}" nil "&aacute;" "a" "á" "á")
    ("Acirc" "\\^{A}" nil "&Acirc;" "A" "Â" "Â")
    ("acirc" "\\^{a}" nil "&acirc;" "a" "â" "â")
    ("Amacr" "\\={A}" nil "&Amacr;" "A" "Ã" "Ã")
    ("amacr" "\\={a}" nil "&amacr;" "a" "ã" "ã")
    ("Atilde" "\\~{A}" nil "&Atilde;" "A" "Ã" "Ã")
    ("atilde" "\\~{a}" nil "&atilde;" "a" "ã" "ã")
    ("Auml" "\\\"{A}" nil "&Auml;" "Ae" "Ä" "Ä")
    ("auml" "\\\"{a}" nil "&auml;" "ae" "ä" "ä")
    ("Aring" "\\AA{}" nil "&Aring;" "A" "Å" "Å")
    ("AA" "\\AA{}" nil "&Aring;" "A" "Å" "Å")
    ("aring" "\\aa{}" nil "&aring;" "a" "å" "å")
    ("AElig" "\\AE{}" nil "&AElig;" "AE" "Æ" "Æ")
    ("aelig" "\\ae{}" nil "&aelig;" "ae" "æ" "æ")
    ("Ccedil" "\\c{C}" nil "&Ccedil;" "C" "Ç" "Ç")
    ("ccedil" "\\c{c}" nil "&ccedil;" "c" "ç" "ç")
    ("Egrave" "\\`{E}" nil "&Egrave;" "E" "È" "È")
    ("egrave" "\\`{e}" nil "&egrave;" "e" "è" "è")
    ("Eacute" "\\'{E}" nil "&Eacute;" "E" "É" "É")
    ("eacute" "\\'{e}" nil "&eacute;" "e" "é" "é")
    ("Ecirc" "\\^{E}" nil "&Ecirc;" "E" "Ê" "Ê")
    ("ecirc" "\\^{e}" nil "&ecirc;" "e" "ê" "ê")
    ("Euml" "\\\"{E}" nil "&Euml;" "E" "Ë" "Ë")
    ("euml" "\\\"{e}" nil "&euml;" "e" "ë" "ë")
    ("Igrave" "\\`{I}" nil "&Igrave;" "I" "Ì" "Ì")
    ("igrave" "\\`{i}" nil "&igrave;" "i" "ì" "ì")
    ("Iacute" "\\'{I}" nil "&Iacute;" "I" "Í" "Í")
    ("iacute" "\\'{i}" nil "&iacute;" "i" "í" "í")
    ("Idot" "\\.{I}" nil "&idot;" "I" "İ" "İ")
    ("inodot" "\\i" nil "&inodot;" "i" "ı" "ı")
    ("Icirc" "\\^{I}" nil "&Icirc;" "I" "Î" "Î")
    ("icirc" "\\^{i}" nil "&icirc;" "i" "î" "î")
    ("Iuml" "\\\"{I}" nil "&Iuml;" "I" "Ï" "Ï")
    ("iuml" "\\\"{i}" nil "&iuml;" "i" "ï" "ï")
    ("Ntilde" "\\~{N}" nil "&Ntilde;" "N" "Ñ" "Ñ")
    ("ntilde" "\\~{n}" nil "&ntilde;" "n" "ñ" "ñ")
    ("Ograve" "\\`{O}" nil "&Ograve;" "O" "Ò" "Ò")
    ("ograve" "\\`{o}" nil "&ograve;" "o" "ò" "ò")
    ("Oacute" "\\'{O}" nil "&Oacute;" "O" "Ó" "Ó")
    ("oacute" "\\'{o}" nil "&oacute;" "o" "ó" "ó")
    ("Ocirc" "\\^{O}" nil "&Ocirc;" "O" "Ô" "Ô")
    ("ocirc" "\\^{o}" nil "&ocirc;" "o" "ô" "ô")
    ("Otilde" "\\~{O}" nil "&Otilde;" "O" "Õ" "Õ")
    ("otilde" "\\~{o}" nil "&otilde;" "o" "õ" "õ")
    ("Ouml" "\\\"{O}" nil "&Ouml;" "Oe" "Ö" "Ö")
    ("ouml" "\\\"{o}" nil "&ouml;" "oe" "ö" "ö")
    ("Oslash" "\\O" nil "&Oslash;" "O" "Ø" "Ø")
    ("oslash" "\\o{}" nil "&oslash;" "o" "ø" "ø")
    ("OElig" "\\OE{}" nil "&OElig;" "OE" "OE" "Œ")
    ("oelig" "\\oe{}" nil "&oelig;" "oe" "oe" "œ")
    ("Scaron" "\\v{S}" nil "&Scaron;" "S" "S" "Š")
    ("scaron" "\\v{s}" nil "&scaron;" "s" "s" "š")
    ("szlig" "\\ss{}" nil "&szlig;" "ss" "ß" "ß")
    ("Ugrave" "\\`{U}" nil "&Ugrave;" "U" "Ù" "Ù")
    ("ugrave" "\\`{u}" nil "&ugrave;" "u" "ù" "ù")
    ("Uacute" "\\'{U}" nil "&Uacute;" "U" "Ú" "Ú")
    ("uacute" "\\'{u}" nil "&uacute;" "u" "ú" "ú")
    ("Ucirc" "\\^{U}" nil "&Ucirc;" "U" "Û" "Û")
    ("ucirc" "\\^{u}" nil "&ucirc;" "u" "û" "û")
    ("Uuml" "\\\"{U}" nil "&Uuml;" "Ue" "Ü" "Ü")
    ("uuml" "\\\"{u}" nil "&uuml;" "ue" "ü" "ü")
    ("Yacute" "\\'{Y}" nil "&Yacute;" "Y" "Ý" "Ý")
    ("yacute" "\\'{y}" nil "&yacute;" "y" "ý" "ý")
    ("Yuml" "\\\"{Y}" nil "&Yuml;" "Y" "Y" "Ÿ")
    ("yuml" "\\\"{y}" nil "&yuml;" "y" "ÿ" "ÿ")
    ("fnof" "\\textit{f}" nil "&fnof;" "f" "f" "ƒ")
    ("real" "\\Re" t "&real;" "R" "R" "ℜ")
    ("image" "\\Im" t "&image;" "I" "I" "ℑ")
    ("weierp" "\\wp" t "&weierp;" "P" "P" "℘")
    ("ell" "\\ell" t "&ell;" "ell" "ell" "ℓ")
    ("imath" "\\imath" t "&imath;" "[dotless i]" "dotless i" "ı")
    ("jmath" "\\jmath" t "&jmath;" "[dotless j]" "dotless j" "ȷ")
    ("Alpha" "A" nil "&Alpha;" "Alpha" "Alpha" "Α")
    ("alpha" "\\alpha" t "&alpha;" "alpha" "alpha" "α")
    ("Beta" "B" nil "&Beta;" "Beta" "Beta" "Β")
    ("beta" "\\beta" t "&beta;" "beta" "beta" "β")
    ("Gamma" "\\Gamma" t "&Gamma;" "Gamma" "Gamma" "Γ")
    ("gamma" "\\gamma" t "&gamma;" "gamma" "gamma" "γ")
    ("Delta" "\\Delta" t "&Delta;" "Delta" "Delta" "Δ")
    ("delta" "\\delta" t "&delta;" "delta" "delta" "δ")
    ("Epsilon" "E" nil "&Epsilon;" "Epsilon" "Epsilon" "Ε")
    ("epsilon" "\\epsilon" t "&epsilon;" "epsilon" "epsilon" "ε")
    ("varepsilon" "\\varepsilon" t "&epsilon;" "varepsilon" "varepsilon" "ε")
    ("Zeta" "Z" nil "&Zeta;" "Zeta" "Zeta" "Ζ")
    ("zeta" "\\zeta" t "&zeta;" "zeta" "zeta" "ζ")
    ("Eta" "H" nil "&Eta;" "Eta" "Eta" "Η")
    ("eta" "\\eta" t "&eta;" "eta" "eta" "η")
    ("Theta" "\\Theta" t "&Theta;" "Theta" "Theta" "Θ")
    ("theta" "\\theta" t "&theta;" "theta" "theta" "θ")
    ("thetasym" "\\vartheta" t "&thetasym;" "theta" "theta" "ϑ")
    ("vartheta" "\\vartheta" t "&thetasym;" "theta" "theta" "ϑ")
    ("Iota" "I" nil "&Iota;" "Iota" "Iota" "Ι")
    ("iota" "\\iota" t "&iota;" "iota" "iota" "ι")
    ("Kappa" "K" nil "&Kappa;" "Kappa" "Kappa" "Κ")
    ("kappa" "\\kappa" t "&kappa;" "kappa" "kappa" "κ")
    ("Lambda" "\\Lambda" t "&Lambda;" "Lambda" "Lambda" "Λ")
    ("lambda" "\\lambda" t "&lambda;" "lambda" "lambda" "λ")
    ("Mu" "M" nil "&Mu;" "Mu" "Mu" "Μ")
    ("mu" "\\mu" t "&mu;" "mu" "mu" "μ")
    ("nu" "\\nu" t "&nu;" "nu" "nu" "ν")
    ("Nu" "N" nil "&Nu;" "Nu" "Nu" "Ν")
    ("Xi" "\\Xi" t "&Xi;" "Xi" "Xi" "Ξ")
    ("xi" "\\xi" t "&xi;" "xi" "xi" "ξ")
    ("Omicron" "O" nil "&Omicron;" "Omicron" "Omicron" "Ο")
    ("omicron" "\\textit{o}" nil "&omicron;" "omicron" "omicron" "ο")
    ("Pi" "\\Pi" t "&Pi;" "Pi" "Pi" "Π")
    ("pi" "\\pi" t "&pi;" "pi" "pi" "π")
    ("Rho" "P" nil "&Rho;" "Rho" "Rho" "Ρ")
    ("rho" "\\rho" t "&rho;" "rho" "rho" "ρ")
    ("Sigma" "\\Sigma" t "&Sigma;" "Sigma" "Sigma" "Σ")
    ("sigma" "\\sigma" t "&sigma;" "sigma" "sigma" "σ")
    ("sigmaf" "\\varsigma" t "&sigmaf;" "sigmaf" "sigmaf" "ς")
    ("varsigma" "\\varsigma" t "&sigmaf;" "varsigma" "varsigma" "ς")
    ("Tau" "T" nil "&Tau;" "Tau" "Tau" "Τ")
    ("Upsilon" "\\Upsilon" t "&Upsilon;" "Upsilon" "Upsilon" "Υ")
    ("upsih" "\\Upsilon" t "&upsih;" "upsilon" "upsilon" "ϒ")
    ("upsilon" "\\upsilon" t "&upsilon;" "upsilon" "upsilon" "υ")
    ("Phi" "\\Phi" t "&Phi;" "Phi" "Phi" "Φ")
    ("phi" "\\phi" t "&phi;" "phi" "phi" "ɸ")
    ("varphi" "\\varphi" t "&varphi;" "varphi" "varphi" "φ")
    ("Chi" "X" nil "&Chi;" "Chi" "Chi" "Χ")
    ("chi" "\\chi" t "&chi;" "chi" "chi" "χ")
    ("acutex" "\\acute x" t "&acute;x" "'x" "'x" "𝑥́")
    ("Psi" "\\Psi" t "&Psi;" "Psi" "Psi" "Ψ")
    ("psi" "\\psi" t "&psi;" "psi" "psi" "ψ")
    ("tau" "\\tau" t "&tau;" "tau" "tau" "τ")
    ("Omega" "\\Omega" t "&Omega;" "Omega" "Omega" "Ω")
    ("omega" "\\omega" t "&omega;" "omega" "omega" "ω")
    ("piv" "\\varpi" t "&piv;" "omega-pi" "omega-pi" "ϖ")
    ("varpi" "\\varpi" t "&piv;" "omega-pi" "omega-pi" "ϖ")
    ("partial" "\\partial"
               t
               "&part;"
               "[partial differential]"
               "[partial differential]"
               "∂")
    ("alefsym" "\\aleph" t "&alefsym;" "aleph" "aleph" "ℵ")
    ("aleph" "\\aleph" t "&aleph;" "aleph" "aleph" "ℵ")
    ("gimel" "\\gimel" t "&gimel;" "gimel" "gimel" "ℷ")
    ("beth" "\\beth" t "&beth;" "beth" "beth" "ב")
    ("dalet" "\\daleth" t "&daleth;" "dalet" "dalet" "ד")
    ("ETH" "\\DH{}" nil "&ETH;" "D" "Ð" "Ð")
    ("eth" "\\dh{}" nil "&eth;" "dh" "ð" "ð")
    ("THORN" "\\TH{}" nil "&THORN;" "TH" "Þ" "Þ")
    ("thorn" "\\th{}" nil "&thorn;" "th" "þ" "þ")
    ("dots" "\\dots{}" nil "&hellip;" "..." "..." "…")
    ("cdots" "\\cdots{}" t "&ctdot;" "..." "..." "⋯")
    ("hellip" "\\dots{}" nil "&hellip;" "..." "..." "…")
    ("middot" "\\textperiodcentered{}" nil "&middot;" "." "·" "·")
    ("iexcl" "!`" nil "&iexcl;" "!" "¡" "¡")
    ("iquest" "?`" nil "&iquest;" "?" "¿" "¿")
    ("shy" "\\-" nil "&shy;" "" "" "")
    ("ndash" "--" nil "&ndash;" "-" "-" "–")
    ("mdash" "---" nil "&mdash;" "--" "--" "—")
    ("quot" "\\textquotedbl{}" nil "&quot;" "\"" "\"" "\"")
    ("acute" "\\textasciiacute{}" nil "&acute;" "'" "´" "´")
    ("ldquo" "\\textquotedblleft{}" nil "&ldquo;" "\"" "\"" "“")
    ("rdquo" "\\textquotedblright{}" nil "&rdquo;" "\"" "\"" "”")
    ("bdquo" "\\quotedblbase{}" nil "&bdquo;" "\"" "\"" "„")
    ("lsquo" "\\textquoteleft{}" nil "&lsquo;" "`" "`" "‘")
    ("rsquo" "\\textquoteright{}" nil "&rsquo;" "'" "'" "’")
    ("sbquo" "\\quotesinglbase{}" nil "&sbquo;" "," "," "‚")
    ("laquo" "\\guillemotleft{}" nil "&laquo;" "<<" "«" "«")
    ("raquo" "\\guillemotright{}" nil "&raquo;" ">>" "»" "»")
    ("lsaquo" "\\guilsinglleft{}" nil "&lsaquo;" "<" "<" "‹")
    ("rsaquo" "\\guilsinglright{}" nil "&rsaquo;" ">" ">" "›")
    ("circ" "\\^{}" nil "&circ;" "^" "^" "∘")
    ("vert" "\\vert{}" t "&vert;" "|" "|" "|")
    ("vbar" "|" nil "|" "|" "|" "|")
    ("brvbar" "\\textbrokenbar{}" nil "&brvbar;" "|" "¦" "¦")
    ("S" "\\S" nil "&sect;" "section" "§" "§")
    ("sect" "\\S" nil "&sect;" "section" "§" "§")
    ("P" "\\P{}" nil "&para;" "paragraph" "¶" "¶")
    ("para" "\\P{}" nil "&para;" "paragraph" "¶" "¶")
    ("amp" "\\&" nil "&amp;" "&" "&" "&")
    ("lt" "\\textless{}" nil "&lt;" "<" "<" "<")
    ("gt" "\\textgreater{}" nil "&gt;" ">" ">" ">")
    ("tilde" "\\textasciitilde{}" nil "~" "~" "~" "~")
    ("slash" "/" nil "/" "/" "/" "/")
    ("plus" "+" nil "+" "+" "+" "+")
    ("under" "\\_" nil "_" "_" "_" "_")
    ("equal" "=" nil "=" "=" "=" "=")
    ("asciicirc" "\\textasciicircum{}" nil "^" "^" "^" "^")
    ("dagger" "\\textdagger{}" nil "&dagger;" "[dagger]" "[dagger]" "†")
    ("dag" "\\dag{}" nil "&dagger;" "[dagger]" "[dagger]" "†")
    ("Dagger" "\\textdaggerdbl{}"
              nil
              "&Dagger;"
              "[doubledagger]"
              "[doubledagger]"
              "‡")
    ("ddag" "\\ddag{}" nil "&Dagger;" "[doubledagger]" "[doubledagger]" "‡")
    ("nbsp" "~" nil "&nbsp;" " " " " " ")
    ("ensp" "\\hspace*{.5em}" nil "&ensp;" " " " " " ")
    ("emsp" "\\hspace*{1em}" nil "&emsp;" " " " " " ")
    ("thinsp" "\\hspace*{.2em}" nil "&thinsp;" " " " " " ")
    ("curren" "\\textcurrency{}" nil "&curren;" "curr." "¤" "¤")
    ("cent" "\\textcent{}" nil "&cent;" "cent" "¢" "¢")
    ("pound" "\\pounds{}" nil "&pound;" "pound" "£" "£")
    ("yen" "\\textyen{}" nil "&yen;" "yen" "¥" "¥")
    ("euro" "\\texteuro{}" nil "&euro;" "EUR" "EUR" "€")
    ("EUR" "\\texteuro{}" nil "&euro;" "EUR" "EUR" "€")
    ("dollar" "\\$" nil "$" "$" "$" "$")
    ("USD" "\\$" nil "$" "$" "$" "$")
    ("copy" "\\textcopyright{}" nil "&copy;" "(c)" "©" "©")
    ("reg" "\\textregistered{}" nil "&reg;" "(r)" "®" "®")
    ("trade" "\\texttrademark{}" nil "&trade;" "TM" "TM" "™")
    ("minus" "-" t "&minus;" "-" "-" "−")
    ("pm" "\\textpm{}" nil "&plusmn;" "+-" "±" "±")
    ("plusmn" "\\textpm{}" nil "&plusmn;" "+-" "±" "±")
    ("times" "\\texttimes{}" nil "&times;" "*" "×" "×")
    ("frasl" "/" nil "&frasl;" "/" "/" "⁄")
    ("colon" "\\colon" t ":" ":" ":" ":")
    ("div" "\\textdiv{}" nil "&divide;" "/" "÷" "÷")
    ("frac12" "\\textonehalf{}" nil "&frac12;" "1/2" "½" "½")
    ("frac14" "\\textonequarter{}" nil "&frac14;" "1/4" "¼" "¼")
    ("frac34" "\\textthreequarters{}" nil "&frac34;" "3/4" "¾" "¾")
    ("permil" "\\textperthousand{}"
              nil
              "&permil;"
              "per thousand"
              "per thousand"
              "‰")
    ("sup1" "\\textonesuperior{}" nil "&sup1;" "^1" "¹" "¹")
    ("sup2" "\\texttwosuperior{}" nil "&sup2;" "^2" "²" "²")
    ("sup3" "\\textthreesuperior{}" nil "&sup3;" "^3" "³" "³")
    ("radic" "\\sqrt{\\,}" t "&radic;" "[square root]" "[square root]" "√")
    ("sum" "\\sum" t "&sum;" "[sum]" "[sum]" "∑")
    ("prod" "\\prod" t "&prod;" "[product]" "[n-ary product]" "∏")
    ("micro" "\\textmu{}" nil "&micro;" "micro" "µ" "µ")
    ("macr" "\\textasciimacron{}" nil "&macr;" "[macron]" "¯" "¯")
    ("deg" "\\textdegree{}" nil "&deg;" "degree" "°" "°")
    ("prime" "\\prime" t "&prime;" "'" "'" "′")
    ("Prime" "\\prime{}\\prime" t "&Prime;" "''" "''" "″")
    ("infin" "\\infty" t "&infin;" "[infinity]" "[infinity]" "∞")
    ("infty" "\\infty" t "&infin;" "[infinity]" "[infinity]" "∞")
    ("prop" "\\propto" t "&prop;" "[proportional to]" "[proportional to]" "∝")
    ("propto" "\\propto" t "&prop;" "[proportional to]" "[proportional to]" "∝")
    ("not" "\\textlnot{}" nil "&not;" "[angled dash]" "¬" "¬")
    ("neg" "\\neg{}" t "&not;" "[angled dash]" "¬" "¬")
    ("land" "\\land" t "&and;" "[logical and]" "[logical and]" "∧")
    ("wedge" "\\wedge" t "&and;" "[logical and]" "[logical and]" "∧")
    ("lor" "\\lor" t "&or;" "[logical or]" "[logical or]" "∨")
    ("vee" "\\vee" t "&or;" "[logical or]" "[logical or]" "∨")
    ("cap" "\\cap" t "&cap;" "[intersection]" "[intersection]" "∩")
    ("cup" "\\cup" t "&cup;" "[union]" "[union]" "∪")
    ("smile" "\\smile" t "&smile;" "[cup product]" "[cup product]" "⌣")
    ("frown" "\\frown" t "&frown;" "[Cap product]" "[cap product]" "⌢")
    ("int" "\\int" t "&int;" "[integral]" "[integral]" "∫")
    ("therefore" "\\therefore" t "&there4;" "[therefore]" "[therefore]" "∴")
    ("there4" "\\therefore" t "&there4;" "[therefore]" "[therefore]" "∴")
    ("because" "\\because" t "&because;" "[because]" "[because]" "∵")
    ("sim" "\\sim" t "&sim;" "~" "~" "∼")
    ("cong" "\\cong" t "&cong;" "[approx. equal to]" "[approx. equal to]" "≅")
    ("simeq" "\\simeq" t "&cong;" "[approx. equal to]" "[approx. equal to]" "≅")
    ("asymp" "\\asymp" t "&asymp;" "[almost equal to]" "[almost equal to]" "≈")
    ("approx" "\\approx"
              t
              "&asymp;"
              "[almost equal to]"
              "[almost equal to]"
              "≈")
    ("ne" "\\ne" t "&ne;" "[not equal to]" "[not equal to]" "≠")
    ("neq" "\\neq" t "&ne;" "[not equal to]" "[not equal to]" "≠")
    ("equiv" "\\equiv" t "&equiv;" "[identical to]" "[identical to]" "≡")
    ("triangleq" "\\triangleq"
                 t
                 "&triangleq;"
                 "[defined to]"
                 "[defined to]"
                 "≜")
    ("le" "\\le" t "&le;" "<=" "<=" "≤")
    ("leq" "\\le" t "&le;" "<=" "<=" "≤")
    ("ge" "\\ge" t "&ge;" ">=" ">=" "≥")
    ("geq" "\\ge" t "&ge;" ">=" ">=" "≥")
    ("lessgtr" "\\lessgtr"
               t
               "&lessgtr;"
               "[less than or greater than]"
               "[less than or greater than]"
               "≶")
    ("lesseqgtr" "\\lesseqgtr"
                 t
                 "&lesseqgtr;"
                 "[less than or equal or greater than or equal]"
                 "[less than or equal or greater than or equal]"
                 "⋚")
    ("ll" "\\ll" t "&Lt;" "<<" "<<" "≪")
    ("Ll" "\\lll" t "&Ll;" "<<<" "<<<" "⋘")
    ("lll" "\\lll" t "&Ll;" "<<<" "<<<" "⋘")
    ("gg" "\\gg" t "&Gt;" ">>" ">>" "≫")
    ("Gg" "\\ggg" t "&Gg;" ">>>" ">>>" "⋙")
    ("ggg" "\\ggg" t "&Gg;" ">>>" ">>>" "⋙")
    ("prec" "\\prec" t "&pr;" "[precedes]" "[precedes]" "≺")
    ("preceq" "\\preceq"
              t
              "&prcue;"
              "[precedes or equal]"
              "[precedes or equal]"
              "≼")
    ("preccurlyeq" "\\preccurlyeq"
                   t
                   "&prcue;"
                   "[precedes or equal]"
                   "[precedes or equal]"
                   "≼")
    ("succ" "\\succ" t "&sc;" "[succeeds]" "[succeeds]" "≻")
    ("succeq" "\\succeq"
              t
              "&sccue;"
              "[succeeds or equal]"
              "[succeeds or equal]"
              "≽")
    ("succcurlyeq" "\\succcurlyeq"
                   t
                   "&sccue;"
                   "[succeeds or equal]"
                   "[succeeds or equal]"
                   "≽")
    ("sub" "\\subset" t "&sub;" "[subset of]" "[subset of]" "⊂")
    ("subset" "\\subset" t "&sub;" "[subset of]" "[subset of]" "⊂")
    ("sup" "\\supset" t "&sup;" "[superset of]" "[superset of]" "⊃")
    ("supset" "\\supset" t "&sup;" "[superset of]" "[superset of]" "⊃")
    ("nsub" "\\not\\subset"
            t
            "&nsub;"
            "[not a subset of]"
            "[not a subset of"
            "⊄")
    ("sube" "\\subseteq"
            t
            "&sube;"
            "[subset of or equal to]"
            "[subset of or equal to]"
            "⊆")
    ("nsup" "\\not\\supset"
            t
            "&nsup;"
            "[not a superset of]"
            "[not a superset of]"
            "⊅")
    ("supe" "\\supseteq"
            t
            "&supe;"
            "[superset of or equal to]"
            "[superset of or equal to]"
            "⊇")
    ("setminus" "\\setminus" t "&setminus;" "\" " "\"" "⧵")
    ("forall" "\\forall" t "&forall;" "[for all]" "[for all]" "∀")
    ("exist" "\\exists" t "&exist;" "[there exists]" "[there exists]" "∃")
    ("exists" "\\exists" t "&exist;" "[there exists]" "[there exists]" "∃")
    ("nexist" "\\nexists"
              t
              "&exist;"
              "[there does not exists]"
              "[there does not  exists]"
              "∄")
    ("nexists" "\\nexists"
               t
               "&exist;"
               "[there does not exists]"
               "[there does not  exists]"
               "∄")
    ("empty" "\\emptyset" t "&empty;" "[empty set]" "[empty set]" "∅")
    ("emptyset" "\\emptyset" t "&empty;" "[empty set]" "[empty set]" "∅")
    ("isin" "\\in" t "&isin;" "[element of]" "[element of]" "∈")
    ("in" "\\in" t "&isin;" "[element of]" "[element of]" "∈")
    ("notin" "\\notin"
             t
             "&notin;"
             "[not an element of]"
             "[not an element of]"
             "∉")
    ("ni" "\\ni" t "&ni;" "[contains as member]" "[contains as member]" "∋")
    ("nabla" "\\nabla" t "&nabla;" "[nabla]" "[nabla]" "∇")
    ("ang" "\\angle" t "&ang;" "[angle]" "[angle]" "∠")
    ("angle" "\\angle" t "&ang;" "[angle]" "[angle]" "∠")
    ("perp" "\\perp" t "&perp;" "[up tack]" "[up tack]" "⊥")
    ("parallel" "\\parallel" t "&parallel;" "||" "||" "∥")
    ("sdot" "\\cdot" t "&sdot;" "[dot]" "[dot]" "⋅")
    ("cdot" "\\cdot" t "&sdot;" "[dot]" "[dot]" "⋅")
    ("lceil" "\\lceil" t "&lceil;" "[left ceiling]" "[left ceiling]" "⌈")
    ("rceil" "\\rceil" t "&rceil;" "[right ceiling]" "[right ceiling]" "⌉")
    ("lfloor" "\\lfloor" t "&lfloor;" "[left floor]" "[left floor]" "⌊")
    ("rfloor" "\\rfloor" t "&rfloor;" "[right floor]" "[right floor]" "⌋")
    ("lang" "\\langle" t "&lang;" "<" "<" "⟨")
    ("rang" "\\rangle" t "&rang;" ">" ">" "⟩")
    ("langle" "\\langle" t "&lang;" "<" "<" "⟨")
    ("rangle" "\\rangle" t "&rang;" ">" ">" "⟩")
    ("hbar" "\\hbar" t "&hbar;" "hbar" "hbar" "ℏ")
    ("mho" "\\mho" t "&mho;" "mho" "mho" "℧")
    ("larr" "\\leftarrow" t "&larr;" "<-" "<-" "←")
    ("leftarrow" "\\leftarrow" t "&larr;" "<-" "<-" "←")
    ("gets" "\\gets" t "&larr;" "<-" "<-" "←")
    ("lArr" "\\Leftarrow" t "&lArr;" "<=" "<=" "⇐")
    ("Leftarrow" "\\Leftarrow" t "&lArr;" "<=" "<=" "⇐")
    ("uarr" "\\uparrow" t "&uarr;" "[uparrow]" "[uparrow]" "↑")
    ("uparrow" "\\uparrow" t "&uarr;" "[uparrow]" "[uparrow]" "↑")
    ("uArr" "\\Uparrow" t "&uArr;" "[dbluparrow]" "[dbluparrow]" "⇑")
    ("Uparrow" "\\Uparrow" t "&uArr;" "[dbluparrow]" "[dbluparrow]" "⇑")
    ("rarr" "\\rightarrow" t "&rarr;" "->" "->" "→")
    ("to" "\\to" t "&rarr;" "->" "->" "→")
    ("rightarrow" "\\rightarrow" t "&rarr;" "->" "->" "→")
    ("rArr" "\\Rightarrow" t "&rArr;" "=>" "=>" "⇒")
    ("Rightarrow" "\\Rightarrow" t "&rArr;" "=>" "=>" "⇒")
    ("darr" "\\downarrow" t "&darr;" "[downarrow]" "[downarrow]" "↓")
    ("downarrow" "\\downarrow" t "&darr;" "[downarrow]" "[downarrow]" "↓")
    ("dArr" "\\Downarrow" t "&dArr;" "[dbldownarrow]" "[dbldownarrow]" "⇓")
    ("Downarrow" "\\Downarrow" t "&dArr;" "[dbldownarrow]" "[dbldownarrow]" "⇓")
    ("harr" "\\leftrightarrow" t "&harr;" "<->" "<->" "↔")
    ("leftrightarrow" "\\leftrightarrow" t "&harr;" "<->" "<->" "↔")
    ("hArr" "\\Leftrightarrow" t "&hArr;" "<=>" "<=>" "⇔")
    ("Leftrightarrow" "\\Leftrightarrow" t "&hArr;" "<=>" "<=>" "⇔")
    ("crarr" "\\hookleftarrow" t "&crarr;" "<-'" "<-'" "↵")
    ("hookleftarrow" "\\hookleftarrow" t "&crarr;" "<-'" "<-'" "↵")
    ("arccos" "\\arccos" t "arccos" "arccos" "arccos" "arccos")
    ("arcsin" "\\arcsin" t "arcsin" "arcsin" "arcsin" "arcsin")
    ("arctan" "\\arctan" t "arctan" "arctan" "arctan" "arctan")
    ("arg" "\\arg" t "arg" "arg" "arg" "arg")
    ("cos" "\\cos" t "cos" "cos" "cos" "cos")
    ("cosh" "\\cosh" t "cosh" "cosh" "cosh" "cosh")
    ("cot" "\\cot" t "cot" "cot" "cot" "cot")
    ("coth" "\\coth" t "coth" "coth" "coth" "coth")
    ("csc" "\\csc" t "csc" "csc" "csc" "csc")
    ("deg" "\\deg" t "&deg;" "deg" "deg" "deg")
    ("det" "\\det" t "det" "det" "det" "det")
    ("dim" "\\dim" t "dim" "dim" "dim" "dim")
    ("exp" "\\exp" t "exp" "exp" "exp" "exp")
    ("gcd" "\\gcd" t "gcd" "gcd" "gcd" "gcd")
    ("hom" "\\hom" t "hom" "hom" "hom" "hom")
    ("inf" "\\inf" t "inf" "inf" "inf" "inf")
    ("ker" "\\ker" t "ker" "ker" "ker" "ker")
    ("lg" "\\lg" t "lg" "lg" "lg" "lg")
    ("lim" "\\lim" t "lim" "lim" "lim" "lim")
    ("liminf" "\\liminf" t "liminf" "liminf" "liminf" "liminf")
    ("limsup" "\\limsup" t "limsup" "limsup" "limsup" "limsup")
    ("ln" "\\ln" t "ln" "ln" "ln" "ln")
    ("log" "\\log" t "log" "log" "log" "log")
    ("max" "\\max" t "max" "max" "max" "max")
    ("min" "\\min" t "min" "min" "min" "min")
    ("Pr" "\\Pr" t "Pr" "Pr" "Pr" "Pr")
    ("sec" "\\sec" t "sec" "sec" "sec" "sec")
    ("sin" "\\sin" t "sin" "sin" "sin" "sin")
    ("sinh" "\\sinh" t "sinh" "sinh" "sinh" "sinh")
    ("sup" "\\sup" t "&sup;" "sup" "sup" "sup")
    ("tan" "\\tan" t "tan" "tan" "tan" "tan")
    ("tanh" "\\tanh" t "tanh" "tanh" "tanh" "tanh")
    ("bull" "\\textbullet{}" nil "&bull;" "*" "*" "•")
    ("bullet" "\\textbullet{}" nil "&bull;" "*" "*" "•")
    ("star" "\\star" t "*" "*" "*" "⋆")
    ("lowast" "\\ast" t "&lowast;" "*" "*" "∗")
    ("ast" "\\ast" t "&lowast;" "*" "*" "*")
    ("odot" "\\odot" t "o" "[circled dot]" "[circled dot]" "ʘ")
    ("oplus" "\\oplus" t "&oplus;" "[circled plus]" "[circled plus]" "⊕")
    ("otimes" "\\otimes" t "&otimes;" "[circled times]" "[circled times]" "⊗")
    ("check" "\\checkmark" t "&checkmark;" "[checkmark]" "[checkmark]" "✓")
    ("checkmark" "\\checkmark" t "&check;" "[checkmark]" "[checkmark]" "✓")
    ("ordf" "\\textordfeminine{}" nil "&ordf;" "_a_" "ª" "ª")
    ("ordm" "\\textordmasculine{}" nil "&ordm;" "_o_" "º" "º")
    ("cedil" "\\c{}" nil "&cedil;" "[cedilla]" "¸" "¸")
    ("oline" "\\overline{~}" t "&oline;" "[overline]" "¯" "‾")
    ("uml" "\\textasciidieresis{}" nil "&uml;" "[diaeresis]" "¨" "¨")
    ("zwnj" "\\/{}" nil "&zwnj;" "" "" "‌")
    ("zwj" "" nil "&zwj;" "" "" "‍")
    ("lrm" "" nil "&lrm;" "" "" "‎")
    ("rlm" "" nil "&rlm;" "" "" "‏")
    ("smiley" "\\ddot\\smile" t "&#9786;" ":-)" ":-)" "☺")
    ("blacksmile" "\\ddot\\smile" t "&#9787;" ":-)" ":-)" "☻")
    ("sad" "\\ddot\\frown" t "&#9785;" ":-(" ":-(" "☹")
    ("frowny" "\\ddot\\frown" t "&#9785;" ":-(" ":-(" "☹")
    ("clubs" "\\clubsuit" t "&clubs;" "[clubs]" "[clubs]" "♣")
    ("clubsuit" "\\clubsuit" t "&clubs;" "[clubs]" "[clubs]" "♣")
    ("spades" "\\spadesuit" t "&spades;" "[spades]" "[spades]" "♠")
    ("spadesuit" "\\spadesuit" t "&spades;" "[spades]" "[spades]" "♠")
    ("hearts" "\\heartsuit" t "&hearts;" "[hearts]" "[hearts]" "♥")
    ("heartsuit" "\\heartsuit" t "&heartsuit;" "[hearts]" "[hearts]" "♥")
    ("diams" "\\diamondsuit" t "&diams;" "[diamonds]" "[diamonds]" "◆")
    ("diamondsuit" "\\diamondsuit" t "&diams;" "[diamonds]" "[diamonds]" "◆")
    ("diamond" "\\diamondsuit" t "&diamond;" "[diamond]" "[diamond]" "◆")
    ("Diamond" "\\diamondsuit" t "&diamond;" "[diamond]" "[diamond]" "◆")
    ("loz" "\\lozenge" t "&loz;" "[lozenge]" "[lozenge]" "⧫")
    ("_ " "\\hspace*{0.5em}" nil "&ensp;" " " " " " ")
    ("_  " "\\hspace*{1.0em}" nil "&ensp;&ensp;" "  " "  " "  ")
    ("_   " "\\hspace*{1.5em}" nil "&ensp;&ensp;&ensp;" "   " "   " "   ")
    ("_    " "\\hspace*{2.0em}"
             nil
             "&ensp;&ensp;&ensp;&ensp;"
             "    "
             "    "
             "    ")
    ("_     " "\\hspace*{2.5em}"
              nil
              "&ensp;&ensp;&ensp;&ensp;&ensp;"
              "     "
              "     "
              "     ")
    ("_      " "\\hspace*{3.0em}"
               nil
               "&ensp;&ensp;&ensp;&ensp;&ensp;&ensp;"
               "      "
               "      "
               "      ")
    ("_       " "\\hspace*{3.5em}"
                nil
                "&ensp;&ensp;&ensp;&ensp;&ensp;&ensp;&ensp;"
                "       "
                "       "
                "       ")
    ("_        " "\\hspace*{4.0em}"
                 nil
                 "&ensp;&ensp;&ensp;&ensp;&ensp;&ensp;&ensp;&ensp;"
                 "        "
                 "        "
                 "        ")
    ("_         " "\\hspace*{4.5em}"
                  nil
                  "&ensp;&ensp;&ensp;&ensp;&ensp;&ensp;&ensp;&ensp;&ensp;"
                  "         "
                  "         "
                  "         ")
    ("_          " "\\hspace*{5.0em}"
                   nil
                   "&ensp;&ensp;&ensp;&ensp;&ensp;&ensp;&ensp;&ensp;&ensp;&ensp;"
                   "          "
                   "          "
                   "          ")
    ("_           " "\\hspace*{5.5em}"
                    nil
                    "&ensp;&ensp;&ensp;&ensp;&ensp;&ensp;&ensp;&ensp;&ensp;&ensp;&ensp;"
                    "           "
                    "           "
                    "           ")
    ("_            " "\\hspace*{6.0em}"
                     nil
                     "&ensp;&ensp;&ensp;&ensp;&ensp;&ensp;&ensp;&ensp;&ensp;&ensp;&ensp;&ensp;"
                     "            "
                     "            "
                     "            ")
    ("_             " "\\hspace*{6.5em}"
                      nil
                      "&ensp;&ensp;&ensp;&ensp;&ensp;&ensp;&ensp;&ensp;&ensp;&ensp;&ensp;&ensp;&ensp;"
                      "             "
                      "             "
                      "             ")
    ("_              " "\\hspace*{7.0em}"
                       nil
                       "&ensp;&ensp;&ensp;&ensp;&ensp;&ensp;&ensp;&ensp;&ensp;&ensp;&ensp;&ensp;&ensp;&ensp;"
                       "              "
                       "              "
                       "              ")
    ("_               " "\\hspace*{7.5em}"
                        nil
                        "&ensp;&ensp;&ensp;&ensp;&ensp;&ensp;&ensp;&ensp;&ensp;&ensp;&ensp;&ensp;&ensp;&ensp;&ensp;"
                        "               "
                        "               "
                        "               ")
    ("_                " "\\hspace*{8.0em}"
                         nil
                         "&ensp;&ensp;&ensp;&ensp;&ensp;&ensp;&ensp;&ensp;&ensp;&ensp;&ensp;&ensp;&ensp;&ensp;&ensp;&ensp;"
                         "                "
                         "                "
                         "                ")
    ("_                 " "\\hspace*{8.5em}"
                          nil
                          "&ensp;&ensp;&ensp;&ensp;&ensp;&ensp;&ensp;&ensp;&ensp;&ensp;&ensp;&ensp;&ensp;&ensp;&ensp;&ensp;&ensp;"
                          "                 "
                          "                 "
                          "                 ")
    ("_                  " "\\hspace*{9.0em}"
                           nil
                           "&ensp;&ensp;&ensp;&ensp;&ensp;&ensp;&ensp;&ensp;&ensp;&ensp;&ensp;&ensp;&ensp;&ensp;&ensp;&ensp;&ensp;&ensp;"
                           "                  "
                           "                  "
                           "                  ")
    ("_                   " "\\hspace*{9.5em}"
                            nil
                            "&ensp;&ensp;&ensp;&ensp;&ensp;&ensp;&ensp;&ensp;&ensp;&ensp;&ensp;&ensp;&ensp;&ensp;&ensp;&ensp;&ensp;&ensp;&ensp;"
                            "                   "
                            "                   "
                            "                   ")
    ("_                    " "\\hspace*{10.0em}"
                             nil
                             "&ensp;&ensp;&ensp;&ensp;&ensp;&ensp;&ensp;&ensp;&ensp;&ensp;&ensp;&ensp;&ensp;&ensp;&ensp;&ensp;&ensp;&ensp;&ensp;&ensp;"
                             "                    "
                             "                    "
                             "                    "))
  "Entity names and their Org export representations.")

(defun other-object-character-p (text position character end)
  (and (< position end) (char= (char text position) character)))

(defun other-object-prefix-p (text start end prefix)
  (and (<= (+ start (length prefix)) end)
       (loop for index below (length prefix)
             always (char= (char text (+ start index)) (char prefix index)))))

(defun other-object-ascii-letter-p (character)
  (and character
       (or (<= (char-code #\A) (char-code character) (char-code #\Z))
           (<= (char-code #\a) (char-code character) (char-code #\z)))))

(defun other-object-letter-p (character)
  (and character (alpha-char-p character)))

(defun other-object-word-character-p (character)
  (and character (or (alpha-char-p character) (digit-char-p character))))

(defun other-object-active-start-p (start)
  (or
   (and (integerp *object-parse-range-start*)
        (= start *object-parse-range-start*))
   (and (null *object-parse-range-start*) (zerop start))))

(defun other-object-word-boundary-p (text start)
  (or (other-object-active-start-p start)
      (zerop start)
      (not (other-object-word-character-p (char text (1- start))))))

(defun other-object-whitespace-p (character)
  (and character (member character '(#\Space #\Tab #\Newline #\Return #\Page))))

(defun other-object-skip-horizontal-space (text position end)
  (loop while (and (< position end)
                   (member (char text position) '(#\Space #\Tab)))
        do (incf position)
        finally (return position)))

(defun other-object-match (type end
                                &key
                                contents-begin
                                contents-end
                                (post-blank 0)
                                properties
                                opaque-p)
  (%make-object-match :type
                      type
                      :end
                      end
                      :contents-begin
                      contents-begin
                      :contents-end
                      contents-end
                      :post-blank
                      post-blank
                      :properties
                      properties
                      :opaque-p
                      opaque-p))

(defun other-object-post-blank-match (text type
                                           object-end
                                           end
                                           properties
                                           &key
                                           contents-begin
                                           contents-end
                                           opaque-p)
  (let ((match-end (other-object-skip-horizontal-space text object-end end)))
    (other-object-match type
                        match-end
                        :contents-begin
                        contents-begin
                        :contents-end
                        contents-end
                        :post-blank
                        (- match-end object-end)
                        :properties
                        properties
                        :opaque-p
                        opaque-p)))

(defun other-object-escaped-p (text position)
  (let ((slashes 0))
    (loop for index downfrom (1- position) to 0
          while (char= (char text index) #\\)
          do (incf slashes))
    (oddp slashes)))

(defun other-object-paired-span (text opening
                                      end
                                      open-character
                                      close-character)
  "Return contents-begin, contents-end and end for a balanced bracket pair."
  (when (other-object-character-p text opening open-character end)
    (let ((depth 1))
      (loop for position from (1+ opening) below
            end
            for character = (char text position)
            unless (other-object-escaped-p text position)
              do (cond
                   ((char= character open-character) (incf depth))
                   ((char= character close-character)
                     (decf depth)
                     (when (zerop depth)
                       (return (values (1+ opening) position (1+ position))))))))))

(defun other-object-nonblank-string (text start end)
  (let ((value (subseq text start end)))
    (unless (every #'other-object-whitespace-p (coerce value 'list))
      value)))

(defun other-object-normalize-header (text start end)
  (let* ((raw (subseq text start end))
         (trimmed (string-trim '(#\Space #\Tab #\Newline #\Return #\Page) raw)))
    (when (plusp (length trimmed))
      (with-output-to-string (output)
        (loop with position = 0
              while (< position (length trimmed))
              for character = (char trimmed position)
              do (if (member character '(#\Newline #\Return))
                     (progn
                       (write-char #\Space output)
                       (incf position)
                       (loop while (and (< position (length trimmed))
                                        (member (char trimmed position)
                                                '(#\Newline #\Return
                                                            #\Space
                                                            #\Tab)))
                             do (incf position)))
                     (progn
                       (write-char character output)
                       (incf position))))))))

(defun other-object-read-paired-text (text position end opener closer)
  (multiple-value-bind (contents-begin contents-end after)
      (other-object-paired-span text position end opener closer)
    (when after
      (values (other-object-nonblank-string text contents-begin contents-end)
              contents-begin
              contents-end
              after))))

(defun other-object-entity-name-at (text start end)
  (let* ((name-start (1+ start))
         (name-end
          (cond
            ((and (< name-start end) (char= (char text name-start) #\_))
             (loop for position from (1+ name-start) below
                   end
                   while (char= (char text position) #\Space)
                   finally (return
                            (if (> position (1+ name-start))
                                position
                                (1+ name-start)))))
            ((other-object-prefix-p text name-start end "there4")
             (+ name-start 6))
            ((and (other-object-prefix-p text name-start end "sup")
                  (< (+ name-start 3) end)
                  (find (char text (+ name-start 3)) "123")) (+ name-start 4))
            ((and (other-object-prefix-p text name-start end "frac")
                  (< (+ name-start 5) end)
                  (find (char text (+ name-start 4)) "13")
                  (find (char text (+ name-start 5)) "24")) (+ name-start 6))
            (t
             (loop for position from name-start below
                   end
                   while (other-object-ascii-letter-p (char text position))
                   finally (return position)))))
         (name (and (> name-end name-start) (subseq text name-start name-end)))
         (following-is-braces
          (and name
               (<= (+ name-end 2) end)
               (other-object-prefix-p text name-end end "{}")))
         (boundary
          (and name
               (or (= name-end end)
                   following-is-braces
                   (not (other-object-letter-p (char text name-end))))))
         (entry (and boundary (assoc name *org-entity-table* :test #'string=))))
    (when entry
      (values entry name-end (not (null following-is-braces))))))

(defun other-object-try-entity (text start end)
  (multiple-value-bind (entry name-end use-brackets-p)
      (other-object-entity-name-at text start end)
    (when entry
      (let* ((token-end
              (if use-brackets-p
                  (+ name-end 2)
                  name-end))
             (match-end (other-object-skip-horizontal-space text token-end end)))
        (other-object-match :entity
                            match-end
                            :post-blank
                            (- match-end token-end)
                            :properties
                            (list :name
                                  (first entry)
                                  :latex
                                  (second entry)
                                  :latex-math-p
                                  (third entry)
                                  :html
                                  (fourth entry)
                                  :ascii
                                  (fifth entry)
                                  :utf-8
                                  (seventh entry)
                                  :use-brackets-p
                                  use-brackets-p)
                            :opaque-p
                            t)))))

(defun other-object-find-string (text value start end)
  (let ((value-length (length value)))
    (loop for position from start to (- end value-length)
          when (other-object-prefix-p text position end value)
            return position)))

(defun other-object-parse-latex-command (text start end)
  (let ((position (1+ start)))
    (loop while (and (< position end)
                     (other-object-ascii-letter-p (char text position)))
          do (incf position))
    (when (> position (1+ start))
      (when (and (< position end) (char= (char text position) #\*))
        (incf position))
      (loop while (< position end)
            for opener = (char text position)
            for closer = (case opener
                           (#\[ #\])
                           (#\{ #\}))
            while closer
            do (let ((close-position
                      (loop for candidate from (1+ position) below
                            end
                            for character = (char text candidate)
                            when (and (not (char= character closer))
                                      (or (char= character #\Newline)
                                          (and (char= opener #\[)
                                               (find character "[]{}"))
                                          (and (char= opener #\{)
                                               (find character "{}"))))
                              return nil
                            when (char= character closer)
                              return candidate)))
                 (if close-position
                     (setf position (1+ close-position))
                     (return))))
      position)))

(defun other-object-dollar-fragment-end (text start end)
  (cond
    ((and (< (1+ start) end) (char= (char text (1+ start)) #\$))
     (let ((closing (other-object-find-string text "$$" (+ start 2) end)))
       (and closing (+ closing 2))))
    ((and (or (= start 0) (not (char= (char text (1- start)) #\$)))
          (< (1+ start) end)
          (not
           (member (char text (1+ start))
                   '(#\Space #\Tab #\Newline #\Return #\, #\. #\;))))
     (loop for closing from (1+ start) below
           end
           for previous = (char text (1- closing))
           for following-is-valid = (or (= (1+ closing) end)
                                        (let ((following
                                               (char text (1+ closing))))
                                          (or
                                           (other-object-whitespace-p following)
                                           (find following "()[]{}.,;:!?\"'"))))
           when (and (char= (char text closing) #\$)
                     (not
                      (member previous
                              '(#\Space #\Tab #\Newline #\Return #\, #\.)))
                     following-is-valid)
             return (1+ closing)))))

(defun other-object-parse-latex-fragment (text start end)
  (let ((fragment-end
         (cond
           ((char= (char text start) #\$)
            (other-object-dollar-fragment-end text start end))
           ((and (< (1+ start) end)
                 (char= (char text start) #\\)
                 (char= (char text (1+ start)) #\())
            (let ((closing
                   (other-object-find-string text "\\)" (+ start 2) end)))
              (and closing (+ closing 2))))
           ((and (< (1+ start) end)
                 (char= (char text start) #\\)
                 (char= (char text (1+ start)) #\[))
            (let ((closing
                   (other-object-find-string text "\\]" (+ start 2) end)))
              (and closing (+ closing 2))))
           ((and (< (1+ start) end)
                 (char= (char text start) #\\)
                 (other-object-ascii-letter-p (char text (1+ start))))
            (other-object-parse-latex-command text start end)))))
    (when fragment-end
      (let ((match-end
             (other-object-skip-horizontal-space text fragment-end end)))
        (other-object-match :latex-fragment
                            match-end
                            :post-blank
                            (- match-end fragment-end)
                            :properties
                            (list :value (subseq text start fragment-end))
                            :opaque-p
                            t)))))

(defun other-object-parse-line-break (text start end)
  (when
      (and (<= (+ start 2) end)
           (char= (char text start) #\\)
           (char= (char text (1+ start)) #\\)
           (or (zerop start) (not (char= (char text (1- start)) #\\))))
    (let* ((after-slashes
            (other-object-skip-horizontal-space text (+ start 2) end))
           (line-end
            (cond
              ((= after-slashes end) after-slashes)
              ((char= (char text after-slashes) #\Newline) (1+ after-slashes))
              ((and (char= (char text after-slashes) #\Return)
                    (< (1+ after-slashes) end)
                    (char= (char text (1+ after-slashes)) #\Newline))
               (+ after-slashes 2))
              ((char= (char text after-slashes) #\Return) (1+ after-slashes)))))
      (when line-end
        (other-object-match :line-break line-end :opaque-p t)))))

(defun other-object-parse-statistics-cookie (text start end)
  (when (char= (char text start) #\[)
    (let ((position (1+ start)))
      (loop while (and (< position end) (digit-char-p (char text position)))
            do (incf position))
      (when
          (and (< position end)
               (or (char= (char text position) #\%)
                   (char= (char text position) #\/)))
        (let ((kind (char text position)))
          (incf position)
          (when (char= kind #\/)
            (loop while (and (< position end)
                             (digit-char-p (char text position)))
                  do (incf position)))
          (when (and (< position end) (char= (char text position) #\]))
            (incf position)
            (let ((match-end
                   (other-object-skip-horizontal-space text position end)))
              (other-object-match :statistics-cookie
                                  match-end
                                  :post-blank
                                  (- match-end position)
                                  :properties
                                  (list :value
                                        (subseq text start position)
                                        :type
                                        nil)
                                  :opaque-p
                                  t))))))))

(defun other-object-parse-inline-babel-call (text start end)
  (when
      (and (other-object-word-boundary-p text start)
           (other-object-prefix-p text start end "call_"))
    (let* ((name-start (+ start 5))
           (name-end
            (let ((position name-start))
              (loop while (and (< position end)
                               (let ((character (char text position)))
                                 (not
                                  (or
                                   (member character
                                           '(#\Space #\Tab #\Newline #\[))
                                   (= (char-code character) 40)))))
                    do (incf position))
              position)))
      (when
          (and (> name-end name-start)
               (< name-end end)
               (find (char text name-end) "[("))
        (let ((call (subseq text name-start name-end))
              (position name-end)
              (inside-header nil)
              (arguments nil)
              (end-header nil))
          (when (char= (char text position) #\[)
            (multiple-value-bind (_value _contents-begin _contents-end after)
                (other-object-read-paired-text text position end #\[ #\])
              (declare (ignore _value _contents-begin _contents-end))
              (unless after
                (return-from other-object-parse-inline-babel-call
                  nil))
              (setf inside-header (other-object-normalize-header text
                                                                 (1+ position)
                                                                 (1- after))
                    position after)))
          (unless (and (< position end) (char= (char text position) #\())
            (return-from other-object-parse-inline-babel-call
              nil))
          (multiple-value-bind (arguments-value _contents-begin
                                                _contents-end
                                                after)
              (other-object-read-paired-text text position end #\( #\))
            (declare (ignore _contents-begin _contents-end))
            (unless after
              (return-from other-object-parse-inline-babel-call
                nil))
            (setf arguments arguments-value
                  position after))
          (when (and (< position end) (char= (char text position) #\[))
            (multiple-value-bind (_value _contents-begin _contents-end after)
                (other-object-read-paired-text text position end #\[ #\])
              (declare (ignore _value _contents-begin _contents-end))
              (when after
                (setf end-header (other-object-normalize-header text
                                                                (1+ position)
                                                                (1- after))
                      position after))))
          (other-object-post-blank-match text
                                         :inline-babel-call
                                         position
                                         end
                                         (list :call
                                               call
                                               :inside-header
                                               inside-header
                                               :arguments
                                               arguments
                                               :end-header
                                               end-header)
                                         :opaque-p
                                         t))))))

(defun other-object-parse-inline-src-block (text start end)
  (when
      (and (other-object-word-boundary-p text start)
           (other-object-prefix-p text start end "src_"))
    (let* ((language-start (+ start 4))
           (language-end
            (loop for position from language-start below
                  end
                  while (not
                         (find (char text position)
                               '(#\Space #\Tab #\Newline #\[ #\{)))
                  finally (return position))))
      (when (> language-end language-start)
        (let ((language (subseq text language-start language-end))
              (position language-end)
              (parameters nil)
              (value nil))
          (when (and (< position end) (char= (char text position) #\[))
            (multiple-value-bind (_value _contents-begin _contents-end after)
                (other-object-read-paired-text text position end #\[ #\])
              (declare (ignore _value _contents-begin _contents-end))
              (unless after
                (return-from other-object-parse-inline-src-block
                  nil))
              (setf parameters (other-object-normalize-header text
                                                              (1+ position)
                                                              (1- after))
                    position after)))
          (unless (and (< position end) (char= (char text position) #\{))
            (return-from other-object-parse-inline-src-block
              nil))
          (multiple-value-bind (contents-begin contents-end after)
              (other-object-paired-span text position end #\{ #\})
            (unless after
              (return-from other-object-parse-inline-src-block
                nil))
            (setf value (subseq text contents-begin contents-end)
                  position after))
          (other-object-post-blank-match text
                                         :inline-src-block
                                         position
                                         end
                                         (list :language
                                               language
                                               :value
                                               value
                                               :parameters
                                               parameters)
                                         :opaque-p
                                         t))))))

(defun other-object-parse-sub-super (text start end type marker)
  (when
      (and (> start 0)
           (or (not (integerp *object-parse-range-start*))
               (> start *object-parse-range-start*))
           (not (other-object-whitespace-p (char text (1- start))))
           (char= (char text start) marker)
           (< (1+ start) end))
    (let ((contents-begin (1+ start))
          (contents-end nil)
          (object-end nil)
          (bracketed nil))
      (cond
        ((find (char text (1+ start)) "{(")
         (let* ((opener (char text (1+ start)))
                (closer
                 (if (char= opener #\{)
                     #\}
                     #\))))
           (multiple-value-bind (_begin content-end after)
               (other-object-paired-span text (1+ start) end opener closer)
             (declare (ignore _begin))
             (when after
               (setf bracketed t
                     contents-begin (+ start 2)
                     contents-end content-end
                     object-end after)))))
        (t
         (let ((position (1+ start)))
           (when (member (char text position) '(#\+ #\-))
             (incf position))
           (loop while (and (< position end)
                            (let ((character (char text position)))
                              (or (alpha-char-p character)
                                  (digit-char-p character)
                                  (find character ".,\\"))))
                 do (when
                        (or (alpha-char-p (char text position))
                            (digit-char-p (char text position)))
                      (setf contents-end (1+ position))) (incf position))
           (when contents-end
             (setf object-end contents-end)))))
      (when object-end
        (let ((match-end
               (other-object-skip-horizontal-space text object-end end)))
          (other-object-match type
                              match-end
                              :contents-begin
                              contents-begin
                              :contents-end
                              contents-end
                              :post-blank
                              (- match-end object-end)
                              :properties
                              (list :use-brackets-p bracketed)))))))

(defun other-object-parse-table-cell (text start end container-type)
  (when
      (and (eq container-type :table-row)
           (or (other-object-active-start-p start)
               (and (> start 0) (char= (char text (1- start)) #\|))))
    (let ((line-end
           (loop for position from start below
                 end
                 when (find (char text position) '(#\Newline #\Return))
                   return position
                 finally (return end))))
      (let* ((separator
              (loop for position from start below line-end
                    when (char= (char text position) #\|)
                      return position))
             (cell-content-end (or separator line-end))
             (cell-end
              (if separator
                  (1+ separator)
                  line-end))
             (contents-begin
              (loop for position from start below line-end
                    while (member (char text position) '(#\Space #\Tab))
                    finally (return position)))
             (contents-end
              (loop for position downfrom cell-content-end above contents-begin
                    while (member (char text (1- position)) '(#\Space #\Tab))
                    finally (return position))))
        (other-object-match :table-cell
                            cell-end
                            :contents-begin
                            contents-begin
                            :contents-end
                            contents-end
                            :properties
                            (list :value nil))))))

(defun try-parse-other-object (document start end container-type)
  "Recognize one non-link, non-markup object at START, within END."
  (let* ((text (document-text document))
         (cell-match
          (other-object-parse-table-cell text start end container-type)))
    (or cell-match
        (case (char text start)
          (#\\
           (or (other-object-parse-line-break text start end)
               (other-object-try-entity text start end)
               (other-object-parse-latex-fragment text start end)))
          (#\$ (other-object-parse-latex-fragment text start end))
          (#\[ (other-object-parse-statistics-cookie text start end))
          (#\_ (other-object-parse-sub-super text start end :subscript #\_))
          (#\^ (other-object-parse-sub-super text start end :superscript #\^))
          (#\c (other-object-parse-inline-babel-call text start end))
          (#\s (other-object-parse-inline-src-block text start end))))))
