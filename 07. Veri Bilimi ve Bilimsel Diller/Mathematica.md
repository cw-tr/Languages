# Mathematica (Wolfram Language)

## Özet
Mathematica (Wolfram Dilinin Kalbi); 1988 yılında dahi teorik fizikçi Stephen Wolfram tarafından kurulan, sırf Rakamlardan ibaret olan programlamayı bırakıp sembolik Matematik, Türev, İntegral denklemlerini cebirsel olarak bir kâğıtta çözüyormuşçasına devasa bir "Her Şeyi Hesaplayan Bilgi Makinesi (Symbolic AI Computation)" felsefesiyle tasarlanmış, dünyanın en büyük felsefik ve bilimsel Lisp/Fonksiyonel tabanlı süper-dilidir.

## Nedir ve Ne İşe Yarar?
Eğer diğer programlama dilleriyle (C veya Python) bir İntegral (Türev/İntegral) çözmek isterseniz, bilgisayara rakamlarla döngüler atıp onu "Sayısal (Numeric)" bir tahmine indirgersiniz (Ona $x=2$ verirseniz size $4.55$ diye bir sayı döndürür). 

Fakat Mathematica ve onun çekirdeği olan **Wolfram Language**; sayısal DEĞİL "Sembolik" bir dildir. Yani siz Mathematica'ya $\int x^2 \,dx$ denklemini yazdığınızda (komut olarak), Size makine ondalıklı sayı döngüsüne girmez, bir insan hoca gibi arka plandaki o devasa sembolik zekasını kullanarak EKRANA doğrudan o lise formülünü basar: **$x^3/3 + C$**. Denklemleri harfleriyle birlikte hesaplar, geometrileri çizer, ve fizik evrenindeki her molekülü "Bilinen tüm bilgiler ansiklopedisi" ile sunar.

**Ne İşe Yarar?**
* **Sembolik Çözümleme ve Fizik Mühendisliği:** Lisedeki, Üniversitedeki 4 katlı diferansiyel denklemlerin (Örn: Maxwel Denklemleri) "X ve Y harfleri" kullanarak kanıtlarını çıkaran Otonom Fizikçi aracıdır.
* **Wolfram Alpha (Arama Motoru):** Google'dan farklı çalışan, sorduğunuz denklemleri veya "New York'un 1995'deki nem oranı ile İstanbul'un nufusu grafiği" sorusunu KENDİ KENDİNE HESAPLAYIP YAPAY ZEKAYLA ÇİZEN Efsanevi Motorun kendi ana dilidir! (Apple Siri, sorularını arka planda bu dil üzerinden matematiğe dökerek cevaplar).

## Dilin Mantığı ve Kod Yapısı
Tamamen (Parantezleri azaltılmış) **Kökten Lisp**'tir lakin syntaxı tamamen matematiktir. Bütün dahili fonksiyon isimleri BÜYÜK HARFLE (Örn: `Plot`, `Integrate`, `Solve`) ve argümanları her zaman **KÖŞELİ PARANTEZ `[ ]`** ile verilir (Çünkü yuvarlak parantezler Lisedeki gibi dümdüz matematiksel çarpım kıyaslaması içindir).

Her şey fonksiyonel ve her şey "Pattern Matching (Şablon eşleştirme)" üzerine kuruludur. Ayrıca o kadar devrimseldir ki Notebook (Defter/Hücre) kavramını ilk getiren sistemdir. Siz bir koda "Bana grafik çiz ve 3D Gezegen yap" dersiniz, kodun hemen altında saniyesinde çevirebildiğiniz kocaman bir 3D Evren Topu belirir (Jupyter Notebooks, fikrini 15 yıl sonra Mathematica'dan kopyalamış/çalmıştır).

**Örnek İşleyiş (Sembolik Olarak):**
Python Döngüsü: Denklemi çözmek için sympy paketi yüklemek ve satırlar atamak.
Wolfram (Mathematica): `Solve[x^2 + 5x + 6 == 0, x]` derseniz size bir saniyede `{x -> -3, x -> -2}` lise çözüm kâğıdını felsefi olarak fırlatır!

### Örnek Bir Mathematica (Wolfram Language) Kodu: Sembolik Matematiğin Tanrısı
Bilgisayarları "Rakam hamallığından" çıkarıp, Sembol Çözen Cebirsel Sanatçıya çeviren ve 3D görsellerle ekrana harika eğriler çizen klasik fonksiyon dizileri:

```mathematica
(* Mathematica (Wolfram Dilinde) Mükemmel Yorum Satirlari (* ve *) isaretleri arasindadir *)
(* Klasik Yazilim Degil: Notebook (Defter) icerisindeki Hucrelerde [Shift + Enter] ile calistirilir *)

(* 1. SEMBOLİK MATEMATİK ZEKASI (Solve ve Integrate) *)

(* Mathematica'ya (X veya Y) nin ne oldugunu soylemenize (var x=5 demenize) GEREK YOKTUR.
   O, X'i bir edebiyat "Sembolu" olarak hafizasinda sonsuza dek tutar! *)

(* Su Karmaşik 2 Bilinmeyenli Denklemi (İki esittir '==' koyarak kiyasla) Lise Ogretmeni gibi ÇÖZ (Solve): *)
denklemCozumu = Solve[{x + y == 10, 2 x - y == 5}, {x, y}]
(* Ekrana(Asagi Çiktiya Basilan Sembolik Kagit): {{x -> 5, y -> 5}} *)


(* C ve Python dillerinin (Döngü atmadan) ağlayarak yapamadıgı o muazzam Integral İspatı: *)
(* Diyoruz ki: a*x^2 + b'nin "X"'e göre sembolik Türev İntegralini al! (a ve b rakam DEĞİLDİR, semboldür!) *)
integralSembolikCikinti = Integrate[a x^2 + b, x]
(* Sistemin firlattigi Cevap: b x + (a x^3) / 3   (Vay canina!) *)


(* 2. YAPAY ZEKA VE VERİ SÖMÜRME (Dahili Ansiklopedi - Wolfram Knowledgebase) *)
(* Dilin icinde EVRENDEKI her seyin verisi(Internete bagli) gomuludur! Apilere ihtiyaciniz yoktur.*)

(* Soru : Bana Dunyadaki Tum Gezegenlerin "Agirlik / Kutle" LİSTESİNİ Ver (EntityValue): *)
gezegenKutleleri = EntityValue[EntityClass["Planet", "All"], "Mass"]
(* Cikti: {Masa[Guney_Kutbu..], JupiterninAgirligi...} Liste halinde aninda verimi patlatir *)


(* 3. DEĞİŞKENLİ GÖRSELLEŞTİRME VE (FONKSİYON FABRIKASI) / PLOT *)
(* Her sey Fonksiyondur! X^2 yi -10 ile 10 arasinda Ciz (Plot): *)

Plot[Sin[x] Exp[-0.1 x], {x, 0, 20}, 
 
 (* Bu harika Çizgiyi(Egriyi) Kırmızı ve Kalin (Thick) Yap. Wolfram bunu Ekranda Grafik olarak cizer! *)
 PlotStyle -> {Red, Thick}, 
 PlotLabel -> "Sönümlenen Dalga (Sembolik Fizik)"]

(* Ayni cizimi 3 Boyutlu Ciz (Plot3D) Dağ gibi Goster! : *)
Plot3D[Sin[x*y], {x, -3, 3}, {y, -3, 3}, ColorFunction -> "Rainbow"]
```
Mathematica, C kodu derlemesi gibi arka planda görünmez değildir, yazdığınız her kod satırının dibine "Matematik hocasının kâğıt kalemiyle" yaptığı o eşsiz analizleri dizip gösterir. 

## Kimler Kullanır?
* Evrendeki bütün akademik **Fizik Bölümleri (Kuantum Fiziği modellemesi vb.) ve İleri Seviye Matematik Bölümleri**. (Fizikçiler C veya Java yazmaz, tüm formüllerini Sembolik olarak Wolfram'dan geçirir).
* Evrimi simüle eden Biyologlar ve Diferansiyel Denklem Çizen (Makine/Elektrik) mühendisleri.
* Veri Biliminde "Stephen Wolfram" kültüne inanan harika bir kitle; ancak Aşırı kapalı-kaynak (Ticari/Lisanslı) oluşundan dolayı günümüzde Yapay Zeka (AI ve Deep Learning) ekosisteminin devasa pazarını açık-kaynak olan Python Pytorch (Google/Meta) devlerine kaptırmıştır, yine de Akademinin tahtındaki Sembolik Kilit taşıdır.
