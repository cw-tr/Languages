# Prolog

## Özet
Prolog (Programming in Logic); 1972 yılında Fransız bilim insanı Alain Colmerauer ve Philippe Roussel tarafından icat edilen, evrendeki diğer tüm popüler dillerden (C, Java, Python vb.) tamamen farklı düşünen; programa "nasıl yapacağını" değil "ne olduğunu ve kuralları" söyleyerek sonucun doğrudan Mantıksal Kanıt Çıkarım Motoru (Inference Engine) ile Yapay Zeka tarafından bulunmasını sağlayan **Deklaratif(Bildirimsel) ve Mantıksal (Logical)** programlama dilidir.

## Nedir ve Ne İşe Yarar?
Eğer C dilinde veya Python'da bir kişinin diğerinin Dedesi olup olmadığını bulmak isterseniz, döngü (`for`) kullanır, ağaç (Tree) mimarisi yazar ve listeleri teker teker RAM'de aratarak kendiniz bir dedektif gibi C'ye her adımı yaptırırsınız (Buna Emredici/Imperative - Nasıl yapacağını söylemek denir).

Prolog'da bilgisayara sadece **Gerçekleri (Facts)** ve **Kuralları (Rules)** öğretirsiniz. Tıpkı bir Aristo felsefesi kitabındaki gibi. Söylersiniz: "Ali, Mehmet'in Babasıdır." Sonra bir Kural söylersiniz: "Eğer X Y'nin Babasıysa, ve Y de Z'nin Babasıysa; o halde X, Z'nin Dedesidir." Prolog'a derlediğinizde gidip **"Ali, Ahmet'in dedesi mi?"** diye sorarsanız (Query/Sorgu), Prolog arka planda kendi muazzam geriye doğru iz sürme (Backtracking) algoritmasıyla veritabanını tarar ve size dümdüz "YES(Evet)" ya da "NO(Hayır)" döner. Her adımı kendisi keşfeder!

**Ne İşe Yarar?**
* **Erken Dönem Sembolik Yapay Zeka (AI) ve Uzman Sistemler:** 1980'lerde ve 90'larda Yapay zekaya, "Eğer hastanın ateşi varsa, Ve öksürüyorsa -> Hastalık Griptir" gibi devasa binlerce kanıt ve kural yüklenerek Erken Tıp Uzman Sistemleri (Expert Systems) yazmak için (LISP ile beraber) yegâne güçlü dildi. Şimdiki Makine Öğreniminden (İstatistik/Olasılık AI'dan) önceki Sembolik/Mantık (Kural Bazlı) Yapay Zekânın kralıdır.
* **Doğal Dil İşleme (Siri / Watson ataları):** İnsan cümlelerindeki (Özne - Yüklem - Nesne) gramer yapılarını birbirinden ayırmak ve çıkarım yapmak için icat edildi. Bugün bile gelişmiş dil ayrıştırma projelerinde geri planda varlığını korutur. IBM'in meşhur Watson Yapay Zekası, ilk yıllarında Prolog kod bloklarından yardım almıştır.

## Dilin Mantığı ve Kod Yapısı
Tamamen **Gerçeklere (Facts)**, **Kurallara (Rules)** ve **Sorgulara (Queries)** dayalıdır! İf-Else komutu kullanmaz, Döngü(For) denen şey literatürde ASLA var olamaz. Bir algoritma çalıştırmak isterseniz "Recursive (Kendi içine geri dönen)" mantıksal gerçekler tanımlamak zorundasınızdır.

Değişkenler `BÜYÜK HARFLE` veya `_` ile başlar. Sabit Bilgiler (Atomlar / Obje isimleri) ise `küçük harfle` başlar. Noktalama işaretleri matematiğin kendisidir:
* Virgül ( `,` ) = VE (AND) mantık bağıdır.
* Noktalı Virgül ( `;` ) = VEYA (OR) mantık bağıdır.
* Nokta ( `.` ) = Bu iddia kapanmıştır / bitmiştir anlamındadır.

**Örnek İşleyiş (Sembolik Olarak):**
Java'da: `if (a > 5 && b < 10)`
Prolog'da: `guzel_saat(X) :- X > 5, X < 10.` (Açıkçası "X'in 5'ten büyük VE(,) 10'dan küçük olduğu şartlarda güzel_saat iddiası doğrudur(:-)" diyerek felsefe yapmaktır).

### Örnek Bir Prolog Kodu: Mantıksal Çıkarım (Yapay Zeka Motorunu Sınamak)
Bir aile soyağacını Python gibi döngülerle dizmek yerine Aristo mantığıyla (Tümevarım / Kanıt Çıkarımı) kurmak ve bilgisayarın KENDİ KENDİNE çözüm bulması (Backtracking):

```prolog
% Prolog dilinde Yorumlar yüzde isareti (%) ile baslar.
% 1. BOLUM: GERCEKLER (Facts - Knowlegde Base)
% Veritabanina (Mantıksal AI beyin sistemine) Sabit kanitlari anlatalim:
% Okunusu: "ahmet, ali'nin babasidir." (Obje adlari kucuk harftir).
baba(ahmet, ali).
baba(ahmet, mehmet).
baba(ali, veli).
baba(mehmet, ayse).

erkek(ahmet).
erkek(ali).
erkek(mehmet).
erkek(veli).

kadin(ayse).

% 2. BOLUM: KURALLAR (Rules - AI Engine)
% Simdi Sisteme Bilgelik ekleyelim.
% ":-" isareti (EGER / SUPPOSE) anlamina gelir. BÜYÜK HARFLER Degiskendir.
% Okunusu: "EGER; X, y'nin babası ise VE (,) X ayni zamanda erkekse, X Y'nin Ogludur iddiası dogrudur!"

oglan_cocugu(Y, X) :- 
    baba(X, Y), 
    erkek(Y).  % (,) Virgul VE islemidir. Nokta bitirir.

% DUNYANIN EN MANTIKLI KURALI: Dede kimdir?
% Okunusu: Eger (:-), X, AraBirinin (Z) babasi ise VE (,) o AraKisi (Z) de Y'nin babasiysa;
% o zaman Kanitlanmistir ki: X, Y'nin dedesidir!

dede(X, Y) :- 
    baba(X, Z), 
    baba(Z, Y). 
```

Bu program sisteme Yüklendiği an BİTER. Bilgisayar hiçbir şey çalıştırmaz, köşede bekler (Bilge kişi gibi). Daha sonra terminale (REPL / Ekranınıza) geçer ve sisteme o an **SORGU (Query)** sorarsınız:

Siz Ekranınızdan Sordunuz: `?- dede(ahmet, veli).` (Ahmet, Veli'nin dedesi mi?)
Prolog 1 Mili-Saniye düşünür ve `true.` (Evet/Doğru) basar.

Siz Ekranınızdan Sordunuz: `?- dede(X, ayse).` (Bana Ayşe'nin Dedesi kimse Onu Bul (X)!)
Prolog arka planda kendi geriye doğru (Backtracking) taramasını yapar, Z şablonlarına uydurur ve saniyeler içinde şunu patlatır: 
`X = ahmet.`

Derleyici (Compiler) sizin için 50 satırlık List-Arama kodunu Aristo mantığı motoruyla çözer.

## Kimler Kullanır?
* C/C++ ve Java ile yazılan yapay zeka kodlarının "Olasılık, Rakam ve Vektör" olmasından (Sıcaklık 0.85 vb) bıkan; saf sembolizm (A, B'dir, C, D'yi gerektirir) algoritmaları çözmek isteyen Semantik Veri Merkezi mimarları ve araştırmacıları.
* Uçuş kontrol sistemlerindeki (Örn: "Eğer Motor 1 arızalı VEYA Basınç Sensörü 2 yanıt vermiyorsa VE Yükseklik 500'den azsa, Kanopi Fırlat") kural motorlarını (Rule Engine) "Kod ile" kanıtlanabilir biçimde yazmak isteyen Uzman Sistem (Expert System) mimarları.
* Kuantum Düşüncesi veya Analitik Geometri Paradoksları çözmeye çalışan akademisyenler. Ancak ticari yazılım (Web, Mobil veya DB) alanında %100 oranında **Ölü ve Terk Edilmiştir.**
