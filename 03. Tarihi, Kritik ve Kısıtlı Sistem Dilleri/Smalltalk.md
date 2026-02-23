# Smalltalk

## Özet
Smalltalk (Smalltalk-80); 1970'lerde Xerox PARC (Palo Alto Research Center) laboratuvarlarında efsanevi bilgisayar bilimcisi Alan Kay ve Dan Ingalls önderliğinde yaratılan, **Dünyanın ilk %100 (Saf) Nesne Yönelimli Programlama (Object-Oriented Programming - OOP)** dilidir ve günümüzde fare, pencere (Windows/Mac) gibi Grafiksel Kullanıcı Arayüzünün (GUI - Graphical User Interface) mucidi, bilgisayar evreninin İkinci Büyük Patlaması (Big Bang)'dır.

## Nedir ve Ne İşe Yarar?
1970'lerde bilgisayarlar "Terminal ekranındaki yeşil yazılar"dan (CMD/Unix) ibaretti. Siyah ekrana satır satır komut giriyordunuz. Alan Kay liderliğindeki dahi ekip bir ütopya gördü: Bilgisayarı 5 yaşındaki çocuklar bile kullanmalı! Bunun için ekranda Pencereler, üzerine tıklanabilen Klasör İkonları, Pop-up'lar ve ekranda hareket eden görünür "Nesneler (Objeler)" inşa ettiler (Steve Jobs ve Bill Gates daha sonra bu konsepti Apple Macintosh ve Windows için çalıp/kopyaladı ve bugün bildiğimiz PC çağı başladı).

İşte o ekrandaki her pencerenin bağımsız olarak birbirine mesaj gönderip hayatta kalabilmesini sağlamak için yaratılan dilin adı **Smalltalk**'tı. 

**Ne İşe Yarar?**
* **Nesnenin Tümüyle Kalbi (Pure OOP):** Simula'nın ektiği tohumlara Alan Kay ismini verdi: OOP. Ancak C++ ya da Java %100 OOP DEĞİLDİR. (Örneğin Java'da `int` veya `5` yavan/tipik bir veridir). Ama Smalltalk'ta evrendeki *HER ŞEY* (Bir sayı `5`, Boşluk `nil`, İf-Else komutunun kedisi Bile) bir OBJEDİR (Sınıftır) ve onlara "Mesaj" gönderirsiniz.
* **Canlı Kodlama (Live Programming) ve IDE'nin İcadı:** Siz C dilini yazar, derler, hata olunca tekrar yazar tekrar derlersiniz. Smalltalk bir "Dil" değil bir **Canlı Ekosistemdir**. Program (Image) çalışırken kodun içindeki butona tıklar; "Koduna Git" der ve O SANİYE motoru durdurmadan kodu düzeltirsiniz; buton anında düzelir! Günümüzdeki modern Debugging (Canlı hata ayıklama / IDE) kavramını programlamaya ilk bunlar armağan etmiştir.

## Dilin Mantığı ve Kod Yapısı
Çizdiği kurgu devrimsel ve günümüz geleneklerine aykırıdır: Fonksiyon(Function) VEYA Prosedür diye bir kavram yoktur! Onun yerine sadece **Kalıplar(Sınıflar)**, ondan fırlatılmış **Nesneler(Objeler)** ve objelerin birbirlerinin içindeki kalkanlı duvarlarına gönderdiği **"Mesajlaşma (Message Passing)"** mekanizmasına dayalıdır! İki nesne birbirleriyle hücre gibi sinyal/mesaj yollyarak konuşur.

Sözdizimi aşırı farklıdır. Çağrılar `Parametre: Deger` keyword harflerle yapılır. Atamalar matematikteki ok `:=` sembolüyle aktarılır ve kod satırları blok blok değil nokta `.` sembolüyle Aristo cümlesi gibi biter.

**Örnek İşleyiş (Sembolik Olarak):**
Örneğin `5 + 3` yazarsanız modern programcı bunun Operatörlü matematik işlemi sanır. Smalltalk derleyicisi ise `5` Objtesine, "Bana içindeki `+` isimli metodu çalıştır! Sana mesaj/yük olarak `3` objesini gönderiyorum, yolla bana" diye bağırır. `if` komutu ifadeden sonraki (Boolean) Objesine yazılan bir *MESAJDIR!*. 

### Örnek Bir Smalltalk Kodu: Sınıf ve Mesaj Yollama (Nesne Devrimi)
Her şeyin bir Sınıf/Nesne kabul edildiği ve metodların boşluk bırakılarak "Mesaj Fırlatması" yapıldığı çok okunabilir ve estetik Pure OOP Cümle(Syntax) yapısı:

```smalltalk
" Smalltalk dilinde Yorumlar cift Tirnak (Quotes) arasinda bulunur. "

" === MESAJ PASLAŞMASI (MESSAGE PASSING) ZEKASI === "

" Basit Mesaj (Unary Message - Objenin Kedine Yollanmasi):
  Matematik Sinificaki(Float) 'Pi' nesnesine git ve o Objeye, bana kendi 'cos' metodunu calistir diye ishal yolla:"
Float pi cos.  " (Java'da bu `Math.cos(Math.PI);` diye yazilirdi) "


" DOKU(SINIF/CLASS) VE INSTANCE Yaratilmasi (Keyword Messages):
  Araba isimli sinifa 'new' Mesaji ilet!"
benimArabam := Araba new. 

" benimArabam Nesnesine (Semsiyesine) 'hiz: ' Mesajini firlat; Argümanı/Kargosu olarak da 120 sayisini (Objesini) ekle! "
benimArabam hiz: 120; ModelAdi: 'Spor Turbo'.

" === DONGU ve IF KONTROLLERI (Kontrol yapisi bile Method'dur/Mesajdir!) === "

" Baska dillerde For/If vardir. Smalltalk'da 'Zaman (Times)' Objesi var!
  Sayı Objesi olan 5'e gidip 'timesRepeat:' Mesajını Fırlat! Yuk olarak Su Blogu [ icindekileri Ekrana Ver ] yolla! "

5 timesRepeat: [
    Transcript show: 'Ben C ve Java degilim, Ben Saf OOP Smalltalkum!'; cr.
    " Transcript Ekrana/Konsola (IDE) yazi yazan Ana Ekrani objesidir. cr(Carriage return) Alt satira in komutudur/mesajidir "
].

" Boolean Degeri Olan True/False (ki o da bir objedir) Mesaj Atalim (If Gbi Davransın): "
(benimArabam hiz > 100) ifTrue: [
    Transcript show: 'Sollama hizindasiniz, ceza gelecek!'; cr.
] ifFalse: [
    Transcript show: 'Kurallara uygun. Devam.'; cr.
].
```
Bu kodlar, 1980 model devasa Xerox/Macintosh Ekranında (Klasörler ve butonlar varken), siz kodu yazar yazmaz anında pencereleri büyütür ve simgeleri oynatırdı!

## Kimler Kullanır?
* 1980 ve 90'lı yıllarda, Amerikan Kurumsal Bankaları (J.P. Morgan) devasa borsa analiz ve finansal-simülasyon sistemleri ile Grafik Arayüzlerini Saniyeler İçinde çizmek (MVC dediğimiz modern Mimarinin yaratıcısıdır Smalltalk aynı zamanda) için yüzlerce geliştiriciye kodlatırırdı.
* Günümüzde **Pharo ve Squeak** (Klasik Smalltalk'ın açık kaynak evrimleri) adında akademi odalarında ve bazı modern Canlı-Medya (Müzik/Arayüz sanatı) kurgusu inşa eden kapalı ve çok niş bir "Sadıklar" zümresi tarafından kullanılır.
* C++, Java, C#, Objective-C (Apple), Ruby Python... Hepsi, bu dilden OOP mantığını aşırdıkları ve "Smalltalk ne başardıysa" onu pratikleştirdikleri için Smalltalk modern yazılımın "Dinozor Fosillerinden" çok "Oksijeni" gibidir, her yerdedir.
