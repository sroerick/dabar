i(defvar bible-abbrevs
  '("Genesis" "Gen." "Gen" "Ge." "Ge"
    "Exodus" "Exo." "Ex." "Exo" "Ex"
    "Leviticus" "Lev." "Lev" "Le." "Le"
    "Numbers" "Num." "Num" "Nu." "Nu"
    "Deuteronomy" "Deut." "Deut" "Deu." "Deu" "De." "De"
    "Joshua" "Josh." "Josh" "Jos." "Jos"
    "Judges" "Judg." "Judg" "Jud." "Jud"
    "Ruth" "Rut." "Rut" "Ru." "Ru"
    "1 Samuel" "1 Sam." "1 Sam" "1 Sa." "1 Sa" "1Sam." "1Sam" "1Sa." "1Sa"
    "2 Samuel" "2 Sam." "2 Sam" "2 Sa." "2 Sa" "2Sam." "2Sam" "2Sa." "2Sa"
    "1 Kings" "1 Kin." "1 Kin" "1 Ki." "1 Ki" "1Kin." "1Kin" "1Ki." "1Ki"
    "2 Kings" "2 Kin." "2 Kin" "2 Ki." "2 Ki" "2Kin." "2Kin" "2Ki." "2Ki"
    "1 Chronicles" "1 Chr." "1 Chr" "1 Ch." "1 Ch" "1Chr." "1Chr" "1Ch." "1Ch"
    "2 Chronicles" "2 Chr." "2 Chr" "2 Ch." "2 Ch" "2Chr." "2Chr" "2Ch." "2Ch"
    "Ezra" "Ezr." "Ezr"
    "Nehemiah" "Neh." "Neh" "Ne." "Ne"
    "Esther" "Esth." "Esth" "Est." "Est" "Es." "Es"
    "Job"
    "Psalms" "Psalm" "Psa." "Psa" "Ps." "Ps"
    "Proverbs" "Prov." "Prov" "Pro." "Pro" "Pr." "Pr"
    "Ecclesiastes" "Eccl." "Eccl" "Ecc." "Ecc" "Ec." "Ec"
    "Song of Songs" "Song of Solomon" "Song" "Son." "Son" "So." "So"
    "Isaiah" "Isa." "Isa" "Is." "Is"
    "Jeremiah" "Jer." "Jer" "Je." "Je"
    "Lamentations" "Lam." "Lam" "La." "La"
    "Ezekiel" "Ezek." "Ezek" "Eze." "Eze"
    "Daniel" "Dan." "Dan" "Da." "Da"
    "Hosea" "Hos." "Hos" "Ho." "Ho"
    "Joel"
    "Amos" "Amo." "Amo" "Am." "Am"
    "Obadiah" "Obad." "Obad" "Oba." "Oba" "Ob." "Ob"
    "Jonah" "Jon." "Jon"
    "Micah" "Mic." "Mic" "Mi." "Mi"
    "Nahum" "Nah." "Nah" "Na." "Na"
    "Habakkuk" "Hab." "Hab"
    "Zephaniah" "Zeph." "Zeph" "Zep." "Zep"
    "Haggai" "Hag." "Hag"
    "Zechariah" "Zech." "Zech" "Zec." "Zec"
    "Malachi" "Mal." "Mal"
    "Matthew" "Matt." "Matt" "Mat." "Mat"
    "Mark" "Mar." "Mar"
    "Luke" "Luk." "Luk" "Lu." "Lu"
    "John" "Joh." "Joh" "Jn." "Jn"
    "Acts of the Apostles" "Acts" "Act." "Act" "Ac." "Ac"
    "Romans" "Rom." "Rom" "Ro." "Ro"
    "1 Corinthians" "1 Cor." "1 Cor" "1 Co." "1 Co" "1Cor." "1Cor" "1Co." "1Co"
    "2 Corinthians" "2 Cor." "2 Cor" "2 Co." "2 Co" "2Cor." "2Cor" "2Co." "2Co"
    "Galatians" "Gal." "Gal" "Ga." "Ga"
    "Ephesians" "Eph." "Eph" "Ep." "Ep"
    "Philippians" "Phil." "Phil"
    "Colossians" "Col." "Col" "Co." "Co"
    "1 Timothy" "1 Tim." "1 Tim" "1 Ti." "1 Ti" "1Tim." "1Tim" "1Ti." "1Ti"
    "2 Timothy" "2 Tim." "2 Tim" "2 Ti." "2 Ti" "2Tim." "2Tim" "2Ti." "2Ti"
    "Titus" "Tit." "Tit"
    "Philemon" "Phlm." "Phlm" "Phl." "Phl"
    "1 Thessalonians" "1 Thess." "1 Thess" "1 Thes." "1 Thes" "1 The." "1 The"
                      "1 Th." "1 Th" "1Thess." "1Thess" "1Thes." "1Thes" "1The."
                      "1The" "1Th." "1Th"
    "2 Thessalonians" "2 Thess." "2 Thess" "2 Thes." "2 Thes" "2 The." "2 The"
                      "2 Th." "2 Th" "2Thess." "2Thess" "2Thes." "2Thes" "2The."
                      "2The" "2Th." "2Th"
    "Hebrews" "Hebr." "Hebr" "Heb." "Heb" "He." "He"
    "James" "Jam." "Jam" "Jas." "Jas" "Ja." "Ja"
    "1 Peter" "1 Pet." "1 Pet" "1 Pe." "1 Pe" "1Pet." "1Pet" "1Pe." "1Pe"
    "2 Peter" "2 Pet." "2 Pet" "2 Pe." "2 Pe" "2Pet." "2Pet" "2Pe." "2Pe"
    "1 John" "1 Joh." "1 Joh" "1 Jo." "1 Jo" "1 Jn." "1 Jn"
             "1Joh." "1Joh" "1Jo." "1Jo" "1Jn." "1Jn"
    "2 John" "2 Joh." "2 Joh" "2 Jo." "2 Jo" "2 Jn." "2 Jn"
             "2Joh." "2Joh" "2Jo." "2Jo" "2Jn." "2Jn"
    "3 John" "3 Joh." "3 Joh" "3 Jo." "3 Jo" "3 Jn." "3 Jn"
             "3Joh." "3Joh" "3Jo." "3Jo" "3Jn." "3Jn"
    "Jude" "Jud." "Jud"
    "Revelation" "Rev." "Rev" "Re." "Re"
    "Apocalypse" "Apoc." "Apoc" "Apo." "Apo" "Ap." "Ap")
  "Abbreviations taken from csebold/esv.el")