# Haskell

## Özet
Haskell; 1990'larda tamamen akademisyenler ve matematikçiler tarafından yaratılmış, yazılım dünyasının en zorlu ama kodları en **Saf (Pure) ve Matematiksel** olan, yan etkisiz (Side-effect free) efsanevi **Fonksiyonel Programlama** dilidir.

## Nedir ve Ne İşe Yarar?
C, Java, Python gibi diller "Buyurucu (Imperative)" dillerdir; bilgisayara adım adım *NASIL* yapacağını söylersiniz ("Şu değişkene 5 ata, sonra 1 artır"). Haskell ise "Bildirimsel (Declarative)" bir dildir; bilgisayara sadece *NEYİ* istediğinizi doğrudan bir matematik formülü gibi yazarsınız.

En büyük felsefesi **Değişmezlik (Immutability)** ve **Saf Fonksiyonlar (Pure Functions)** üzerine kuruludur. Haskell'de bir değişken bir kez `X = 5` olduysa, program kapanana kadar bir daha asla ama asla `X = 6` olamaz. Hafızayı (State) değiştiremezsiniz!

**Ne İşe Yarar?**
* **Sıfır Yan Etki (No Side-Effects):** Bir fonksiyon dışarıdaki hiçbir Global değişkeni okuyamaz, hiçbir yeri bozamaz, ekrana gizlice log basıp sistem dosyasını bile okuyamaz (özel bir kılıf olan Monad'lar haricinde). Bu yüzden yazılan kodun, uzay havacılığında veya askeri alanda "Kesinlikle Matematiksel Olarak Kanıtlanmış Doğruluk" (Formal Verification) sunması garantidir.
* **Eşzamanlılık (Concurrency):** Hiçbir değişken asla sonradan değiştirilemediği için (Immutable), aynı anda 1 Milyon işlemci çekirdeği (Thread) aynı değişkene saldırsa bile veriler kitlenmez, çökmez. Milyonlarca veri paralel işlenir.
* **Devasa Optimizasyon (Tembel Değerlendirme - Lazy Evaluation):** Haskell tembeldir. Ona "Bana sonsuzluğa kadar giden sayıların bir listesini ver" derseniz bilgisayarınız donmaz veya sonsuz döngüye girip RAM bitmez. Çünkü Haskell, siz o sonsuz listenin sadece "ilk 5" elemanını ekrana yazdırmak istediğiniz saniyeye kadar hiçbir şeyi gereksiz (peşin) hesaplamaz.

## Dilin Mantığı ve Kod Yapısı
Döngüler (`for`, `while`) dilde **yoktur**. Bunun yerine her şey birbirini çağırma (Recursion - Özyineleme) ve liste kavrayışlarıyla (List Comprehensions) su gibi akarak hesaplanır.

Kodların okunabilirliği bir yazılımcıya tamamen yabancı, bir matematik profesörüne ise şiir gibidir. Tipler çok sıkıdır, ancak o kadar zekidir ki Java'daki gibi defalarca "bu integer'dır" yazmanıza gerek bırakmaz, Tipi kendi kendine (Type Inference) çözer.

**Örnek İşleyiş (Sembolik Olarak):**
Liste içindeki çift sayıları bulmak için geleneksel (`for i=0; i<liste; i++ if i%2==0`) eziyeti yerine, tıpkı lise matematiğindeki küme kuramı gibi: "{ x | x ∈ Liste, x çifttir }" yazarsınız. O size mükemmel bir hızla ayıklanmış yepyeni bir liste verir.

### Örnek Bir Haskell Kodu: Quicksort (Hızlı Sıralama) Algoritması
C dilinde veya Java'da iç içe döngülerle, temp değişkenleriyle, pointer taklalarıyla dolu 30 satır süren meşhur Quicksort Dizilim (Sıralama) Algoritması'nın Haskell'de Lise kümeler teorisiyle sadece **3 satıra** nasıl sıkıştığına bir bakalım:

```haskell
-- DİKKAT: Haskell'de yorum satırları '--' ile başlar.
-- 1. ADIM (Tip Bildirimi - Opsiyonel ama Şıktır): 
-- İçinde (a) tipi elemanlar olan bir Liste alıp, tekrar (a) tipli bir Liste döneceğini söyler.
quicksort :: (Ord a) => [a] -> [a]

-- 2. ADIM (Algoritma Kalbi - Fonksiyonel Tanım)
-- Kural 1: Eğer bana "boş" bir liste gelirse [] (Temel Durum), geriye aynen boş liste [] döndürür.
quicksort [] = []

-- Kural 2: Eğer içinde en az 1 eleman olan normal liste gelirse (Örn: p = listenin başı/Pivot, xs = listenin kuyruğu)
quicksort (p:xs) = 
    -- Matematik MUCİZESİ burada başlıyor (Sırala ve Birleştir - ++ operatörü birleştirmedir) :
    
    -- a) Pivot'tan KÜÇÜK olan veya ona Eşit Olan tüm sayıları bul (xs içindeki y'ler), ve onları KENDİ İÇİNDE yine quicksort yap.
    let kucukler = quicksort [y | y <- xs, y <= p]
    
    -- b) Pivot'tan BÜYÜK olan tüm sayıları (y'leri) filtrele ve kendi içinde quicksort et.
    let buyukler = quicksort [y | y <- xs, y > p]
    
    -- c) Sonuç: Sıralanmış küçükler LİSTESİ + Pivotun kendisi + Sıralanmış Büyükler LİSTESİ. (Alttaki in bloğu)
    in  kucukler ++ [p] ++ buyukler


-- Programın Ana Başlangıç (IO Monad) Noktası (Ekrana basma ve Kirli dünya ile temas anı)
main = do
    -- Ekrana [1, 2, 3, 5, 8, 9, 10] basacaktır, saf donanım hızıyla:
    print (quicksort [5, 1, 9, 3, 2, 10, 8]) 
```

Bütün sistemin matematiksel "Sırala (küçükler) -> Pivotla Ortada Buluş -> Sırala (büyükler)" şeklinde doğal okunması, yazılım tarihindeki en elit çözümlerden biridir.

## Kimler Kullanır?
* Bankaların Kantitatif Finans (Algorithmic Trading) Mimarları. (Özellikle Barclays gibi bankalar ve Standard Chartered, en karmaşık hata affetmez türev hesaplamalarını milyonlarca veri satırı boyunca Haskell ile kanıtlayarak koşarlar).
* Cardano (ADA) gibi kripto-para ve Akıllı Sözleşme (Smart Contract) blockchain projelerinin arkasındaki kurucu güvenlik yazılımcıları ("Plutus" dili Haskell altyapılıdır).
* C veya Java'nın spagetti yapısından sıkılıp zihinsel aydınlanma (Paradigma Değişimi) arayan saf bilgisayar bilimi araştırmacıları ile akademisyenler.
