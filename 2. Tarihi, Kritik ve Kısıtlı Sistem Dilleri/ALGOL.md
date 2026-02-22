# ALGOL

## Özet
ALGOL (Algorithmic Language); 1958'de Amerika ve Avrupa birliğindeki bilgisayar bilimcilerinin ortak bir konsorsiyumu tarafından tasarlanan, bugün dünyada kullandığımız C, C++, Java, Pascal, Go gibi neredeyse **tüm modern programlama dillerinin "Atası (Büyükbabası)"** sayılan, Blok-Yapılı (Block-Structured) programlamayı icat eden efsanevi dildir.

## Nedir ve Ne İşe Yarar?
1950'lerde diller (Örn: Assembly veya erken Fortran) "Spagetti Kod" dediğimiz şeye sahipti. Kod yukarıdan aşağıya akar, döngü kurmak için `GOTO 50` (50. satıra atla) gibi emirler verilirdi. Bir kod 10.000 satır olduğunda o `GOTO` kelimeleri yüzünden kodun nerede başlayıp nerede bittiği anlaşılamaz, projelere adeta düğüm girerdi.

ALGOL komitesi dünyayı sarsan hedefini açıkladı: "**Blok Yapısı (Block Structure)**". Kodu dev bir kazan yerine "Odacıklara" (Bloklara) böldüler. `begin` ile bir oda başlar, `end` ile biter. O odanın içindeki değişken, dışarıdaki odaya sızamaz, dışarıdaki içeriye karışamazdı (Buna "Lexical Scoping" denildi). Modern dillerdeki süslü parantezlerin `{ }` atası ALGOL'un `begin...end` mantığıdır!

**Ne İşe Yarar?**
* **Algoritma Yayıncılığı:** 1960'larda bilgisayarlar çok pahalı olduğu için bilim insanları algoritmalarını kağıda yazıp dergilere (Örn: ACM) yollardı. Lakin her bilim adamı kendi uydurduğu notasyonu kullanırdı. ALGOL, evrensel ve matematiksel bir "Kağıt Üstü Algoritma İfade Dili" olarak çıkmış ve bilim camiasında 30 yıl boyunca "Standart Kod Yayınlama Dili" olmuştur.
* **Modern Dillerin Taslağı (ALGOL-Like):** Bugün dünyayı yöneten C dili (dolayısıyla C++ ve Java), ALGOL 60 ve daha sonraki BCPL akımının düzeltilmiş ve sadeleştirilmiş kopyalarından türemiştir.

## Dilin Mantığı ve Kod Yapısı
ALGOL tamamen bir devrimdi. İlk defa "İç içe geçmiş Fonksiyonları (Nested Functions)" icat etti. "Rekürsif (Kendi kendini çağıran)" fonksiyonların ilk defa pratik olarak kullanıldığı yapısal dildi. 

Matematiksel bir dizilimi vardı. Değişken ataması (Assignment) için `=` değil, `:=` kullanılırdı (Pascal ve Delphi'nin bunu kimden çaldığını anlamışsınızdır).

**Örnek İşleyiş (Sembolik Olarak):**
Fortran "İf(A.EQ.B)" derken, ALGOL tertemiz `if A = B then begin ... end` diyerek modern `if-else` mantığını da tarihe kazıdı.

### Örnek Bir ALGOL (ALGOL 60) Kodu: Bloklar ve Rekürsif Algoritma
Bir programlama tarihçisinin baktığında C veya Pascal'ın köklerini, `{}` parantezleri yerine "begin-end" in nasıl odacıklara böldüğünü gördüğü Klasik Faktöriyel Algoritması:

```algol
'COMMENT' ALGOL dilinde yorumlar tirnak icinde COMMENT ile veya sonlandirici noktalı virgüle kadar yapilirdi.;

'BEGIN'
  'COMMENT' Burasi Ana (Main) blogun baslangicidir. Sadece buradaki degiskenler lokaldir!;
  
  'INTEGER' i, sonuc;

  'COMMENT' İlk defa Recursion(Öz Yineleme) mantigini destekleyen Muhtesem ALGOL 60 Fonksiyonu:;
  'INTEGER' 'PROCEDURE' Faktoriyel(n);
    'VALUE' n; 'INTEGER' n;
  'BEGIN'
    'IF' n = 0 'THEN'
        Faktoriyel := 1
    'ELSE'
        'COMMENT' Fonksiyon Kendi kendini Cagirabiliyor! (Devrim);
        Faktoriyel := n * Faktoriyel(n - 1)
  'END';

  'COMMENT' PROGRAMIN ASIL YURUTULDUGU YER;
  'FOR' i := 1 'STEP' 1 'UNTIL' 5 'DO'
  'BEGIN'
      sonuc := Faktoriyel(i);
      
      'COMMENT' PrintText ve PrintInt ekrana firlatma komutlaridir;
      PrintText("Faktoriyel: ");
      PrintInt(i);
      PrintText(" is: ");
      PrintInt(sonuc);
      OutLine; 'COMMENT' Alt satira in ;
  'END'
  
'END'
```

Bu `begin` ve `end` kelimeleri, tüm bilgisayar tarihini değiştirdi.

## Kimler Kullanır?
* Kimse kullanmıyor. ALGOL bugün tamamen **ölü bir dildir**. 
* En büyük sebebi, dili yapan Konsorsiyum'un dili "Input / Output (Ekrana veya dosyaya yazdırma)" kuralları eklemeden piyasaya sürmesiydi. "Algoritma dili kağıda yazılır, ekrana değil" diyerek IO kütüphanesi yapmamak büyük vizyonsuzluktu. IBM gibi şirketler komutların birbiriyle nasıl bağlantı kuracağını çözemeyince dili terk ettiler.
* Ancak ruhu; C, Java ve Go dillerinin süslü parantezlerinde her milisaniye yaşamaya devam ediyor.
