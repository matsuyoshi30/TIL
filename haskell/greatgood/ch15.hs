import Data.List (break)

data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Show)

freeTree :: Tree Char
freeTree =
  Node 'P'
    (Node 'O'
      (Node 'L'
        (Node 'N' Empty Empty)
        (Node 'T' Empty Empty)
      )
      (Node 'Y'
        (Node 'S' Empty Empty)
        (Node 'A' Empty Empty)
      )
    )
    (Node 'L'
      (Node 'W'
        (Node 'C' Empty Empty)
        (Node 'R' Empty Empty)
      )
      (Node 'A'
        (Node 'A' Empty Empty)
        (Node 'C' Empty Empty)
      )
    )

-- パンくずリストを作って木構造のデータをたどっていく経路を保存する
data Direction = L | R deriving (Show)
type Breadcrumbs = [Direction]

goLeft :: (Tree a, Breadcrumbs) -> (Tree a, Breadcrumbs)
goLeft (Node _ l _, bs) = (l, L:bs)

goRight :: (Tree a, Breadcrumbs) -> (Tree a, Breadcrumbs)
goRight (Node _ _ r, bs) = (r, R:bs)

-- 木構造のデータを与えると、関数の通りたどっていった先にある部分木を取得できる
-- *Main> goLeft (goRight (freeTree, []))
-- (Node 'W' (Node 'C' Empty Empty) (Node 'R' Empty Empty),[L,R])

-- が、上のままだと部分木から別の部分木に行くときに、パンくずリストが「経路」しか持ってないので、経路を戻って別の部分木に行くことができない（保存できていないので）
-- データ構造を変えてみる

-- 移動元のルートノードと、パンくずリストに乗らない方の部分木を持つようにする
data Crumb a = LeftCrumb a (Tree a) | RightCrumb a (Tree a) deriving (Show)

type Breadcrumbs' a = [Crumb a]

goLeft' :: (Tree a, Breadcrumbs' a) -> (Tree a, Breadcrumbs' a)
goLeft' (Node x l r, bs) = (l, LeftCrumb x r:bs)

goRight' :: (Tree a, Breadcrumbs' a) -> (Tree a, Breadcrumbs' a)
goRight' (Node x l r, bs) = (r, RightCrumb x l:bs)

goUp :: (Tree a, Breadcrumbs' a) -> (Tree a, Breadcrumbs' a)
goUp (t, LeftCrumb x r:bs) = (Node x t r, bs)
goUp (t, RightCrumb x l:bs) = (Node x l t, bs)

-- Tree a と Breadcrumbs' a は、もとの木構造のデータ全体を復元するのに必要な情報に加えて、特定の部分木に注目している状態を表している
-- あるデータにおいて、注目点とその周辺情報（上の例では特定の部分木とその親の木のルートノードおよびその左右どちらかの部分木）を含んでいるデータ構造を Zipper という

type Zipper a = (Tree a, Breadcrumbs' a)

modify :: (a -> a) -> Zipper a -> Zipper a
modify f (Node x l r, bs) = (Node (f x) l r, bs)
modify _ (Empty, bs) = (Empty, bs)

-- 木とZipperを受け取って、注目点を与えられた木で置き換えたZipperを返す
attach :: Tree a -> Zipper a -> Zipper a
attach t (_, bs) = (t, bs)
-- 空（終端ノード）を置換して木を伸ばしたり、既存の木構造データを置き換えたりできる

-- いま注目点がどこかに関わらず、木構造データ全体のルートノードを返す
topMost :: Zipper a -> Zipper a
topMost (t, []) = (t, [])
topMost z = topMost (goUp z)


-- ここまで木構造をみてきたが、リストも1分木としてみなすことができる（先頭と先頭以外の部分リスト）

type ListZipper a = ([a], [a]) -- 注目しているリストと、パンくずリスト

goForward :: ListZipper a -> ListZipper a
goForward (x:xs, bs) = (xs, x:bs)

goBack :: ListZipper a -> ListZipper a
goBack (xs, x:bs) = (x:xs, bs)


-- filesystem
-- ファイル：名前がついていて、データが入っている
-- フォルダ：名前がついていて、複数のファイルやフォルダをアイテムとして持つ

type Name = String
type Data = String
data FSItem = File Name Data | Folder Name [FSItem] deriving (Show)

myDisk :: FSItem
myDisk =
  Folder "root"
    [ File "goat_yelling_like_man.wmv" "baaa"
    , File "pope_time.avi" "god bless"
    , Folder "pics"
        [ File "ape_throwing_up.jpg" "bleafth"
        , File "watermelon_smash.gif" "smash!"
        , File "skull_man(scary).bmp" "Yikes!"
        ]
    , File "dijon_poupou.doc" "beat mustart"
    , Folder "programs"
        [ File "fartwizard.exe" "10gotofart"
        , File "owl_bandit.dmg" "mov eax, h00t"
        , File "not_a_virus.exe" "really not a virust"
        , Folder "source code"
           [ File "beat_hs_prog.hs" "main = print (fix error)"
           , File "random.hs" "main = print 42"
           ]
        ]
    ]

data FSCrumb = FSCrumb Name [FSItem] [FSItem] deriving (Show)
type FSZipper = (FSItem, [FSCrumb])

fsUp :: FSZipper -> FSZipper
fsUp (item, FSCrumb name ls rs:bs) = (Folder name (ls ++ [item] ++ rs), bs)
-- 注目点より前にあったリスト ls と後ろにあったリスト rs をつなげて新しい注目点（フォルダ）を返す

fsTo :: Name -> FSZipper -> FSZipper
fsTo name (Folder folderName items, bs) =
  let (ls, item:rs) = break (nameIs name) items
  in (item, FSCrumb folderName ls rs:bs)
-- 与えられた名前のデータにたどり着くまでファイルシステムを掘っていく

nameIs :: Name -> FSItem -> Bool
nameIs name (Folder folderName _) = name == folderName
nameIs name (File fileName _) = name == fileName

x -: f = f x

-- *Main> let newFocus = (myDisk, []) -: fsTo "pics" -: fsTo "skull_man(scary).bmp"
-- *Main> fst newFocus
-- File "skull_man(scary).bmp" "Yikes!"

fsRename :: Name -> FSZipper -> FSZipper
fsRename newName (Folder name items, bs) = (Folder newName items, bs)
fsRename newName (File name dat, bs) = (File newName dat, bs)

-- *Main> let newFocus = (myDisk, []) -: fsTo "pics" -: fsRename "cspi" -: fsUp
-- *Main> fst newFocus
-- Folder "root" [File "goat_yelling_like_man.wmv" "baaa",File "pope_time.avi" "god bless",Folder "cspi" [File "ape_throwing_up.jpg" "bleafth",File "watermelon_smash.gif" "smash!",File "skull_man(scary).bmp" "Yikes!"],File "dijon_poupou.doc" "beat mustart",Folder "programs" [File "fartwizard.exe" "10gotofart",File "owl_bandit.dmg" "mov eax, h00t",File "not_a_virus.exe" "really not a virust",Folder "source code" [File "beat_hs_prog.hs" "main = print (fix error)",File "random.hs" "main = print 42"]]]

-- フォルダ配下に新規のアイテムを作成する
fsNewFile :: FSItem -> FSZipper -> FSZipper
fsNewFile item (Folder folderName items, bs) = (Folder folderName (item:items), bs)

-- *Main> let newFocus = (myDisk, []) -: fsTo "pics" -: fsNewFile (File "heh.jpg" "lol") -: fsUp
-- *Main> fst newFocus
-- Folder "root" [File "goat_yelling_like_man.wmv" "baaa",File "pope_time.avi" "god bless",Folder "pics" [File "heh.jpg" "lol",File "ape_throwing_up.jpg" "bleafth",File "watermelon_smash.gif" "smash!",File "skull_man(scary).bmp" "Yikes!"],File "dijon_poupou.doc" "beat mustart",Folder "programs" [File "fartwizard.exe" "10gotofart",File "owl_bandit.dmg" "mov eax, h00t",File "not_a_virus.exe" "really not a virust",Folder "source code" [File "beat_hs_prog.hs" "main = print (fix error)",File "random.hs" "main = print 42"]]]

-- 二分木の操作に失敗の文脈をいれてもナディック関数にする
goLeft'' :: Zipper a -> Maybe (Zipper a)
goLeft'' (Node x l r, bs) = Just (l, LeftCrumb x r:bs)
goLeft'' (Empty, _) = Nothing

goRight'' :: Zipper a -> Maybe (Zipper a)
goRight'' (Node x l r, bs) = Just (r, RightCrumb x l:bs)
goRight'' (Empty, _) = Nothing

goUp' :: Zipper a -> Maybe (Zipper a)
goUp' (t, LeftCrumb x r:bs) = Just (Node x t r, bs)
goUp' (t, RightCrumb x l:bs) = Just (Node x l t, bs)
goUp' (_, []) = Nothing -- パンくずリストが空ならもう上には行けない＝ルートノード

-- これらの関数の戻り値が Zipper a から Maybe (Zipper a) になったので、 -: でつなぐことができない
-- >>= をつかう！

-- *Main> let coolTree = Node 1 Empty (Node 3 Empty Empty)
-- *Main> return (coolTree, []) >>= goRight''
-- Just (Node 3 Empty Empty,[RightCrumb 1 Empty])
-- *Main> return (coolTree, []) >>= goRight'' >>= goRight''
-- Just (Empty,[RightCrumb 3 Empty,RightCrumb 1 Empty])
-- *Main> return (coolTree, []) >>= goRight'' >>= goRight'' >>= goRight''
-- Nothing
