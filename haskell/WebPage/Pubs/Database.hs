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

-- ** Institutions
sjtu = "Shanghai Jiao Tong University"
ub   = "University at Buffalo"

-- ** Journals
tetc   = short "TETC" "IEEE Trans, on Emerging Topics in Computing"

-- ** Conferences
globecom = short "Globecom" "IEEE Globe Communications Conference"
infocom  = short "Infocom"  "IEEE International Conference on Computer Communications"
ccs      = short "CCS"      "ACM Conference on Computer and Communications Security"

-- ** Workshops



--
-- * Papers
--

-- ** Lists of papers in chronological order.

y13 = [infocom13, tetc13]
y12 = [globecom12, ccs12poster]

allPubs = concat [y13,y12]

-- ** 2013
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
