# B Dili

## Özet
B Dili (B Language), 1969 civarında efsanevi Bell Laboratuvarları'nda doğrudan Ken Thompson ve Dennis Ritchie tarafından (C dili doğmadan hemen önce) yazılmış, modern sistem programlama dillerinin en önemli ata yadigarlarından biridir. 

## Nedir ve Ne İşe Yarar?
1960'ların sonlarında bilgisayarlar giderek küçülüp "Minibilgisayar" (Örn: PDP-7) formatına geçmeye başladıklarında, Ken Thompson'ın yeni geliştirmekte olduğu efsanevi UNIX işletim sistemini sadece hantal Assembly ile yazması imkânsız hale geliyordu. Daha okunaklı, ama aynı zamanda makine dilinin esnekliğine sahip bir araca ihtiyacı vardı. 

Atası olan `BCPL` dilinden gereksiz her şeyi kırpıp çok ufak belleklere (8 Kilobayt sınırlarına) sığabilecek kadar "daraltılmış ve sıkıştırılmış" olan B Dilini icat etti.

**Ne İşe Yarar?**
* **UNIX'in Doğuşu:** İlk UNIX işletim sisteminin devasa parçaları ve tarihteki ilk faydalı (utility) işletim sistemi komutları B dili ile sıfırdan oluşturulmaya başlanmıştır.
* **Makine Seviyesi Kontrolü:** Tipleri olmayan, doğrudan bellek "Kelimesine" (Machine Word) müdahale eden bu yapı, işletim sistemlerine cihaz donanımı üzerinde hızlı erişim yetkisi tanıdı.

## Dilin Mantığı ve Kod Yapısı
B dili **Tip-siz (Typeless)** bir dildir. Günümüzde alışkın olduğumuz *tam sayı (integer)*, *ondalıklı sayı (float)* ya da *karakter (char)* gibi kavramlar B dilinde yoktur. 

B dili için her bir veri tek bir şeydi: O an kullanılan makinenin (Bilgisayarın CPU'sunun) anlayabildiği doğrudan "Makine Kelimesi" (Örn: PDP-7 için 18 bit uzunluğunda dümdüz bellek parçası). Bu bellek parçasının içindeki sayının, klavyede "A" harfine mi denk geldiğini, yoksa "65" rakamını mı ifade ettiğini dil önemsemezdi. Tamamen programcının o an onu nerede (örneğin matematiksel bir toplama mı yoksa ekrana yazdırma mı) kullandığına göre şekillenirdi.

B dili çok gelişmeye müsait bir yapı değildi. Yıllar geçtikçe farklı minibilgisayarlara farklı boyutlarda (Örn PDP-11 makinesine 16 bitlik veri ve Bayt tabanlı farklı mimari) geçildiği an Typeless (Tipsiz) olması B dilini çaresiz bıraktı. Çünkü hangi verinin "Byte", hangisinin "Integer" olduğunu dilden ayıramıyordunuz. (İşte bu boşluk doldurulup, tipler eklendiği an C Dili doğacaktır).

**Örnek İşleyiş (Sembolik Olarak):**
Eğer bellekte A, B ve C değişkenleri açıyorsanız, içlerine isterseniz harf dizisi (String), isterseniz mantıksal operatör tıkıştırırsınız. Derleyici tipe karışıp size laf etmez. Hafıza, geliştiricinin ellerindeydi (ve çok tehlikeliydi). Ayrıca atama ve karşılaştırma operatörleri bugün kullandığımız `+=` ya da `==` formatlarının ilk kıvılcımlarını atmaya başlamıştı.

### Örnek Bir B Kodu
Bugün hala C dilinde gördüğümüz, ana fonksiyonun `main()` ile başlaması ve fonksiyon yapısının `{...}` ile donanması ilk defa (BCPL'den rafine halde) burada kök salmıştı. Aşağıdaki yapı ekrana çıktı vermeyi sağlar (Eski `printf` benzeri bir komut olan `putchar` gibi sistem fonksiyonlarıyla):

```c
/* B dilinde açıklama satırları yapısı bugünkü gibidir */

main( ) {
  /* Değişken bildirimi oto-tipsiz yapılır, 
     örneğin a ve b, o makine için boş Word alanlarıdır. */
  auto a, b, c;

  a = 5;
  b = 10;
  
  /* B dili değişkenleri işaretçi (pointer) gibi rahatça yönetirdi,
     Ancak bu işaretçiler modern C'deki gibi katı değildi. */
  c = a + b;
  
  /* Bellekte karakter bastırma işlemi veya fonksiyon çağrıları */
  printn(c, 10); 
}

printn(n, b) {
    /* Kendini çağıran döngüler ve aritmetik modüler hesaplar */
    auto a;

    if (a = n / b)       /* Eşitlik testi (bugünkü == değil = idi) ve atama */
        printn(a, b);
    putchar(n % b + '0');
}
```

## Kimler Kullanır?
* 1970 civarlarında Bell Laboratuvarları başta olmak üzere devasa PDP donanımlarına erişimi olan az sayıda dâhi akademik sistem tasarımcıları (Dennis Ritchie ve Brian Kernighan vb.).
* Günümüzde **kimse tarafından "üretime hazır" bir dil olarak kullanılmamaktadır.** Ancak teknoloji tarihi ve derleyici geliştirme felsefesinin (Parser, Lexer ve Bellek Yönetimi) nasıl evrimleştiğini anlamak için üniversite müfredatlarında ve teknoloji müzelerinde atıf alır.
