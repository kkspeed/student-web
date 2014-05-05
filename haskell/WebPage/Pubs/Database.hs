{-# LANGUAGE PatternGuards, TupleSections #-}

module WebPage.Pubs.Database where

import WebPage.Pubs.Paper


--
-- * Defintions
--

-- ** Authors
muyuanli    = Author "Muyuan" "Li"
kuiren      = Author "Kui" "Ren"
sichen      = Author "Si"  "Chen"
haojin      = Author "Haojin" "Zhu"
zhaoyu      = Author "Zhaoyu" "Gao"
suguo       = Author "Suguo"  "Du"
mianxiong   = Author "Mianxiong" "Dong"
kaoru       = Author "Kaoru" "Ota"
yaoliu      = Author "Yao" "Liu"
zhenfu      = Author "Zhenfu" "Cao"
yujintu     = Author "Yujin" "Tu"
chaozhang   = Author "Chao" "Zhang"
binsheng    = Author "Bingsheng" "Zhang"
zhanqin     = Author "Zhan" "Qin"
junfei      = Author "Junfei" "Wang"
congwang    = Author "Cong" "Wang"
dima        = Author "Di" "Ma"

-- ** Institutions
sjtu = "Shanghai Jiao Tong University"
ub   = "University at Buffalo"

-- ** Journals
tetc   = short "TETC" "IEEE Trans, on Emerging Topics in Computing"
iot    = short "IOT"  "IEE, Internet of Things Journa"

-- ** Conferences
globecom = short "Globecom" "IEEE Globe Communications Conference"
infocom  = short "Infocom"  "IEEE International Conference on Computer Communications"
ccs      = short "CCS"      "ACM Conference on Computer and Communications Security"
mobicom  = short "MobiCom"  "Internantional Conference on Mobile Computing and Networking"

-- ** Workshops



--
-- * Papers
--

-- ** Lists of papers in chronological order.

y13 = [priwhisper, infocom13, tetc13, mobicomApp13]
y12 = [globecom12, ccs12poster]

allPubs = concat [y13,y12]

-- ** 2013
priwhisper  = journal "Priwhishper"
              [binsheng, zhanqin, sichen, muyuanli, kuiren, congwang, dima]
              "PriWhisper: Enabling Keyless Secure Acoustic Communication for Smartphones"
              2014
              @@ iot

tetc13      = journal "haojin2013-tetc"
              [haojin, suguo, muyuanli, zhaoyu]
              "Fairness-Aware and Privacy-Preserving Friend Matching Protocol in Mobile Social Networks"
              2013
              `onPages` (Pages 192 200)
              @@ tetc

infocom13   = conference "zhaoyu2012-infocom"
              [zhaoyu, haojin, yaoliu, muyuanli, zhenfu]
              "Location Privacy in Database-driven Cognitive Radio Networks: Attacks and Countermeasures"
              2013
              @@ infocom

mobicomApp13 = other "sichen2013-mobicom"
               [sichen, muyuanli, yujintu, chaozhang, binsheng, zhanqin, junfei, kuiren ]
               "AcousAuth: A smartphone empowered personal authentication system exploiting keyless acoustic communication"
               2013
               `atURL` "http://vimeo.com/77708077"
               `withNote` "App Competition Finalist (Top 10)"
               @@ mobicom
-- ** 2012
ccs12poster = poster "pp22-gao"
              [zhaoyu, haojin, yaoliu, muyuanli, zhenfu]
              "POSTER: Location Privacy Leaking from Spectrum Utilization Information in Database-driven Cognitive Radio Networks"
              2012
              @@ ccs

globecom12 = conference "muyuan2012-globecom"
  [muyuanli, zhaoyu, haojin, suguo, mianxiong, kaoru]
  "PriMatch: Fairness-aware secure friend discovery protocol in mobile social network"
  2012
  @@ globecom
