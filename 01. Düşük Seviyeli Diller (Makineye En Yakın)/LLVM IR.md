# LLVM IR (Intermediate Representation)

## Özet
LLVM IR (Low Level Virtual Machine - Intermediate Representation); 2000'li yıllardan sonra ortaya çıkan, günümüzün milyarlarca dolarlık C++, Rust ve Swift gibi dillerinin "Derleme Arafında (Front-end ile Back-end ortasında)" birbirleriyle ortak anlaştığı, evrensel, katı **sanal makine dili (Ara Temsil Dilidir)**. 

## Nedir ve Ne İşe Yarar?
1990'larda her dilin kendine ait eziyetli bir "Makineye (İşlemciye)" çevrilme motoru vardı. C için ayrı bir eziyet, Fortran için ayrı bir eziyet çekilir ve "Intel" için farklı, "ARM" için farklı derleyici (C programları) yazılırdı. 

LLVM bu kâbusu bitiren sihirli devrin adıdır! C++ kodu, Rust kodu veya Swift kodu (Bunlara Front-end / Ön Yüz denir) önce **LLVM IR** isminde evrensel, melez, çok yüksek optimizasyonlu (Assembly'den bir tık akıllı) "Ara Bir Dile" çevrilir. Intel'in veya Android'in İşlemcisi, Rust'ın ya da C++'ın ne olduğunu bilmez! O sadece bu LLVM IR dosyalarına odaklanır. 

**Ne İşe Yarar?**
* **Dil Evrenselliği (Compiler Infrastructure):** Yeni bir programlama dili yarattıysanız, Milyonlarca Windows/Mac işlemcisine onun mantığını öğretmekle uğraşmazsınız. Kendi programlama dilinizden çıkan kodu (AST'yi) sadece 1 kuralla "**LLVM IR'ye çeviren**" bir program yazarsınız. Gerisini LLVM Projesi sizin için halleder, dünyanın %99 PC'sinde kodunuz en hızlı şekilde native çalışır. Zig, Nim, Swift vb diller tamamen bunun üstünde koşar.
* **Agresif Kod Optimizasyonu (Arka Planda):** İki dil arasındaki LLVM ara motoru inanılmaz bir Yapay Zeka gibidir. C++'ta siz 100 kere dönen döngü yazıp sonucu 500'e bağladıysanız, LLVM IR bunu görür, "Bu döngü boşuna bilgisayarı yoracak" der, döngüyü siler ve LLVM içinden direkt `ans = 500` diye değiştirip makineye o şekilde "Sıfır işlem saatiyle" çakar (SSA Mimarisi).

## Dilin Mantığı ve Kod Yapısı
Tam bir Assembly diline benzemez, ama tipik bir insan programlama diline de benzemez (Yine de okunabilir/metin tabanlıdır - `*.ll` dosyası olarak kaydedilebilir). 

İki inanılmaz özelliği vardır:
1.  **Sonsuz Sanal Kayıtlıdır (Infinite Registers):** Normal Assembly `eax`, `ebx` diye 4-5 küçücük RAM register yuvasına sığmaya çalışır ve döne döne çakışır. LLVM IR ise `%1, %2, %3... %9998` diye arka arkaya bilgisayarda "sonsuz sayıda" küçük kutu açmayı kendine adet edinir (!). Asıl işlemi Donanım (CPU) yaparken İşlemci (Arka uç) bu binlerce `%x` kodunu gerçeğe (4-5 yuvaya) sıkıştırır. Programcı için devasa bir cennettir.
2.  **Statik Tek Atama Mimarisi (SSA - Static Single Assignment):** Bir değişkene (Örn: `%1`) bir değer ("Adım") atandı mı; **BİTTİ, O İSİM BİR DAHA KULLANILAMAZ.** C dilinde `X = 5`, az aşağı in `X = 10` diyerek X'i çöp yapıyordunuz. LLVM bunu yasaklar: `%1 = 5` atadıysan, az aşağı inince `%1 = 10` diyemezsin, mecburen YENİ bir isim `%2 = 10` verip o kutuyu sonsuza dek kilitlersin. Bu, Derleyicinin değişken takibinde "Bug" (Hatayı) sıfıra indiren matematiksel mucizesidir.

**Örnek İşleyiş (Sembolik Olarak):**
Siz C dilinde `int c = a + b;` yazarsınız, Clang (LLVM'in derleyicisi) bunu `%3 = add i32 %1, %2` ("1. Register ile 2. Registeri toplayıp 3'e at, bu da 'i32' tipi yani tam sayı" diyerek IR formatına hapseder. Bütün diller bu köprüde diz çöker.

### Örnek Bir LLVM IR Kodu: 10 + 20'yi Toplayan Saf Araf Motoru
Yazılımcıların asla açıp da manuel yazmadığı (C veya Rust tarafından otomatik üretilen), "TİPLİ, SONSUZ REGİSTERLI" ve "% işareti kullanilan" Assembly evriminin ta kendisi:

```llvm
; LLVM IR'de yorum satırları ; (Noktalı Virgül) ile başlar.
; "@" işareti Global(Açık) foksiyon, "%" işareti Lokal (İç) Fonksiyon/Register'dır.

; "main" ana C fonksiyonun İçi-Dışı Tip tanımlı(i32 = Int32) olarak baştan oluşturulması:
define i32 @main() {

  ; İLK BLOK (Entry):
entry:
  
  ; SSA MANTIGI! 
  ; i32 tipli (Int) 10 rakamını 'alloca' emriyle geçici hafızada sanal bir bellek pointerine (İbresine) kitle:
  %1 = alloca i32
  store i32 10, i32* %1  ; (Sanal %1 ibresi artik %100 oraninda Int(10)'dur!)

  ; i32 tipli 20 rakamını BAŞKA BIR SIFIR(%2) sanal Bellekle kitle:
  %2 = alloca i32
  store i32 20, i32* %2

  ; MATEMATİK ZAMANI (Load and Add): 
  ; Değerleri ibrelerden GERCİ ÇEKİP(Load edip), %3 ve %4 diye YEPYENI Registerlara SSA Mimarisiyle AKTARIYORUZ
  %3 = load i32, i32* %1
  %4 = load i32, i32* %2
  
  ; ADD! (Topla ve %5 isimli YENI SONSUZ SSA YUVASINA hapsol!)
  %5 = add i32 %3, %4

  ; 30 Çıkan sayıyı C Dilindeymiş gibi Ekrana return (geri döndür) Emriyle ver ve işlemi bitir:
  ret i32 %5
}
```

Bu kod C dilinden de gelse, Swift'ten de gelse, Rust'tan da gelse; LLVM o 30 sayısının %5'te toplandığını anlayıp İşlemci donanımınıza "Bu sadece 30'dur, diğerlerine yer açın" diye fısıldar. 

## Kimler Kullanır?
* C/C++ (Clang), Rust, Swift, Julia, Zig, Nim gibi milyonları kasıp kavuran popüler Programlama Dillerinin Derleyici Takımları (Compiler Engineers - Çekirdek Geliştiricileri).
* Apple'ın, Google'ın ve dev oyun/sistem şirketlerinin arka tarafında, programları bilgisayar gücüne dökebilmek ve her mimariye (ARM/x86/M1/M2/RISC-V) şıp diye koşturabilmek için kullandıkları Compiler ekosistemi liderleri. Modern "Araç Zincirinin (Toolchain)" kalbidir.
