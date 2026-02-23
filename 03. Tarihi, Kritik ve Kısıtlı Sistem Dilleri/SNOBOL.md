# SNOBOL

## Özet
SNOBOL (String Oriented and Symbolic Language); 1962 yılında Bell Laboratuvarları'nda (AT&T) geliştirilen, tarihte metin işleme (String manipulation), karakter dizileri ve Şablon Eşleştirme (Pattern Matching) operasyonları üzerine uzmanlaşarak icat edilen ilk ve tek dil olan, modern Regex (Düzenli İfadeler) kavramının manevi babası sayılan efsanevi dildir.

## Nedir ve Ne İşe Yarar?
1960'ların başlarında bilim insanları bilgisayarları (Fortran / Assembly) genellikle sadece "Rakam hesaplamak" veya "Matris Çarpmak" için kullanıyordu. Eğer bir metnin (String'in) içinden belirli bir İngilizce kelimeyi aramak, onu başkasıyla değiştirmek veya dilbilgisi kurallarına göre metni parçalamak isterseniz, bu sayısal dillerde kelimeleri Array(Dizi) gibi harf harf döngülerle gezmeniz, korkunç spagetti kodlar yazmanız gerekiyordu.

Bell Labs ekibi, sadece Kelimelerle oynayabilen bir dil olan SNOBOL'u geliştirdi. SNOBOL, "Düzenli İfade (Regular Expression / Regex)" adı verilen günümüz modern arama motorlarının kök mantığını o yıllarda kendine has bir "Pattern (Şablon) Uydurma" kurallarıyla tek satıra indirgiyordu.

**Ne İşe Yarar?**
* **Doğal Dil İşleme (NLP) ve Dilbilimi:** 1970'lerde bir kitabın metnindeki İngilizce gramer hatalarını bulmak, şifreleri kırmak (Kriptografi) veya veri bankalarından metinsel anahtar kelime avcılığı yapmak için kurulan ana sistemdi.
* Günümüzde dünyada her dilde kullandığımız (Python, JS, C++ dahil) **Regex Motorlarının (Regular Expressions)** yapay zekasına ve felsefesine tapan, onların atasını oluşturan yapıdır.

## Dilin Mantığı ve Kod Yapısı
Çok eşsiz (ve modern gözlere çok çirkin gelen) bir kurgusu vardır. Geleneksel Aritmetik formüller gibi `Sol = Sağ` yerine formülleri **`Değişken  Şablon  =  Değiştirilecek_Yeni_Metin`** şeklinde, arasına eşitlik koymadan boşluklarla dizilerek işletirdi.

Fonksiyonları veya döngüleri yoktu; bunun yerine başarılı olursa atla, başarısız olursa atla manasına gelen `:S(Etiket)` (Success/Başarılı) ve `:F(Etiket)` (Fail/Başarısız) harfleriyle `GOTO` benzeri korkunç zıp zıp zıplayan komut mimarisine sahipti. 

**Örnek İşleyiş (Sembolik Olarak):**
Örneğin `Metin = "Elma armut muz"` dizisinden "armut" kelimesini "çilek" yapmak için:
C'de: `Metnin.replace("armut", "cilek")` 
SNOBOL'da: `Metin 'armut' = 'cilek'` (Hiçbir fonksiyon yok, doğrudan yanına yazarsınız, sihirli bir şekilde kendisi o deseni arar bulur ve silerdi).

### Örnek Bir SNOBOL (SNOBOL4) Kodu: Metin İçinden Deseni (Pattern) Koparmak
Günümüzdeki herhangi bir dilin String Manipülasyon gücünü 60'lı yıllarda, komando gibi boşluklu sözdizimi ve "Success/Fail" GoTo atlamalarıyla çözen metin avcısı:

```snobol
* SNOBOL dilinde Yorumlar yildiz (*) harfi ve bosluk ile baslar!

        * DEĞİŞKEN ATANMASI (Tırnak içi string)
        MESAJ = "DIKKAT: BU MESAJ GIZLI BIR SIFRE BARINDIRIYOR."

        * PATTERN (Şablon) Yaratilmasi:
        * Diyoruz ki; "GIZLI" kelimesini bul, ondan sonraki ILK kelimeyi veya
        * boslugu 'ARB' (Arbitrary - Herhangi bir metin) ile SUC_KELIMESI'ne ata!
        AVCI_SABLON = "GIZLI " ARB . SUC_KELIMESI " BARINDIRIYOR"

        * SİHİRLİ İŞLEM BURADA KOPUYOR: (Eşitliği yok, Fonksiyon yok)
        * Ana metin ile Avcı Şablonu birleştir ve VUR:
ARAMA   MESAJ AVCI_SABLON                     :S(BULUNDU) F(YOK)
        
        * YUVAYA ZIPLAMA ETIKETLERI (GOTO'nun SNOBOL Veriyonu - S(Success) / F(Fail) ):
BULUNDU 
        * OUTPUT, ekrana bastirma emridir
        OUTPUT = "Hedef Basariyla Yakalandi!"
        OUTPUT = "Yakalanan Sifreli Kelime: " SUC_KELIMESI
        :(SON)

YOK     
        OUTPUT = "Maalesef, bu kelimenin icinde Şablon yasamiyor."
        :(SON)

SON     
        OUTPUT = "Program Kapaniyor."
END
```
Bu kod tek satırda stringi ortasından yarar, ortasında kalan belirsiz `ARB` kelimesini cımbızla çeker ve değişkene atardı. 60'larda For ve IF döngüsü yazmadan bu "Oto-Cımbız (Pattern Matching)" büyüsel sayılıyordu.

## Kimler Kullanır?
* 1960 ve 70'lerde beşerî bilimciler (Tarihçiler, Dilbilimciler - M.I.T), makale tarama araçları ve Amerikan ulusal kütüphanecilik derlemeleri için indeks arayan akademisyenler.
* Modern günde SNOBOL **tamamen ölüdür**. Onun yerini 80'lerde önce **AWK** ve **Perl**, günümüzde ise doğrudan **Python** (Regex kütüphaneleriyle) almıştır. Ancak bütün bu String sihirbazları ana fikirlerini SNOBOL mirasından kopyalamışlardır.
