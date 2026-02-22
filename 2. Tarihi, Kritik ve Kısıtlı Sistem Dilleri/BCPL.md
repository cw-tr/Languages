# BCPL

## Özet
BCPL (Basic Combined Programming Language), 1967 yılında İngiltere'deki Cambridge Üniversitesi'nde Martin Richards tarafından geliştirilmiş, günümüz yazılım dünyasının ana çatısını (Süslü parantezler, blok kompozisyonları) oluşturan atalardan biri olan oldukça akademik ve tarihi bir dildir.

## Nedir ve Ne İşe Yarar?
1960'ların ortalarında CPL (Combined Programming Language) adlı aşırı detaylı, devasa ve karmaşık bir dil projesi üzerinde çalışılıyordu. O dönemki donanımların yetersizliği yüzünden bu karmaşık dil bir türlü derleyici (compiler) aşamasını tamamlayamıyordu. Martin Richards, bu hantal projenin en kilit donanımsal kısımlarını makaslayarak, sadece kendi kendine çalışabilen ve temel işlemleri çözebilen "Temel" (Basic) bir versiyonunu yarattı; bu da **BCPL** oldu.

**Ne İşe Yarar?**
* **Sistem Yazılımları ve Derleyici Geliştirme:** Çok kısıtlı bir bellek kapladığı ve oldukça hızlı derlenebilir yapısı olduğu için, farklı cihazlara yeni işletim sistemleri yazmak veya başka gelişmiş programlama dillerinin ilk derleyicilerini inşa etmek amacıyla kullanıldı (Soket görevi).
* **Taşınabilirlik (Portability):** O dönem tüm işletim sistemleri ve uygulamalar spesifik sadece bir bilgisayar donanımına ve mimarisine mühürlenirken (Örn; IBM donanımı), BCPL "O-Code" adı verilen ara bir sanal makine koduna (bugünkü Java Bytecode benzeri bir atılım) derlenip, birçok farklı devasa donanımda taşınabilir olarak çalışabilme devrimini tetikledi.

## Dilin Mantığı ve Kod Yapısı
Tıpkı halefi B diline miras bırakacağı gibi, BCPL de **Tip-siz (Typeless)** bir dildi. Floating point, character, char array gibi veri mimarileri (Data-Types) yoktu. Hafızada (RAM) ayrılan her alan dümdüz bir makine "Word"ünden (Donanım Kelimesi) ibaretti. Bu, programcıya korkutucu düzeyde yüksek donanım esnekliği, bir o kadar da çökme/mavi ekran garantili tehlike riski veriyordu.

Ayrıca, yazılım dünyasında efsanevi **"Merhaba Dünya!" (Hello World)** kavramını ilk defa teknik bir makalede belgelemesiyle de saygıdeğer bir anısı vardır. (Brian Kernighan tarafından BCPL dokümanlarında geçmiştir).

**Örnek İşleyiş (Sembolik Olarak):**
BCPL dünyasında bloklar vardır. O zamana kadar kodu döngüye sokarken Assembly'deki gibi `Şuraya Git (GOTO)` gibi kaotik atlamalar yapılıyordu. BCPL spagetti kodu yıkıp yerine "Başlangıç ve Son aralığını belirten Yapısal Bloklar kur" dedi. Süslü parantezlerin `{ }`, `DO`, `IF..ELSE` gruplamalarının mantıksal doğuş noktasıdır.

### Örnek Bir BCPL Kodu
Dili bugünkü yazılım yapılarına ne kadar benzediğine, ancak fonksiyon ve yapı tanımlamalarının `LET` ekseninde şekillendiğine tanık olun:

```text
// BCPL'de "//" çift slash ile başlayan metinler yorum satırlarıdır. 
// Ayrıca "/* ... */" bloğunu da destekleyen ilk dil budur!

GET "libhdr" // Standart giriş/çıkış makine kütüphanesini içeri al

// Değişken (daha doğrusu fonksiyon bellek alanı) atamaları LET ile başlar
LET start() = VALOF // Ana yürütme fonksiyonu ve Value geri döndüreceğini belli eder.
$( // BCPL ilk etapta blokları $( ... $) parantezleriyle izole ediyordu. Sonra { }'lere evrildi.

    LET fact(n) = n=0 -> 1, n*fact(n-1)  // Özyinelemeli (Recursive) bir Faktöriyel denklemi.
    
    // Klasik bir For Döngüsü prototipi:
    FOR i = 1 TO 5 DO
    $(
        // Ekrana formatlı karakter yazdırma işlemleri ("*N" komutu New-Line yani Vuruş/AltSatır anlamındadır).
        writef("%I2! = %I4*N", i, fact(i))
    $)
    
    RESULTIS 0 // İşletim sistemine sıfır "Sorunsuz Bitti" döndür (return 0).
$)
```

Dilin yapısında fonksiyon bile bir `LET` (belirle/ata) komutuyla hafızada boş bir `Word` değerine işlenip bloklanırdı.

## Kimler Kullanır?
* Bell Laboratuvarları, MIT, ve Cambridge Üniversitesi'ndeki bilgisayar çağı öncüleri (Dennis Ritchie dahil) uzun bir süre bilgisayar mimarisini kurarken kullandı. 
* TRIPOS işletim sisteminin ve ilk "MUD" (Kullanıcı etkileşimli Metin Tabanlı Çevrimiçi oyunlar) ve AmigaDOS gibi mimarilerin kilit yapılarında kaldı.
* Günümüzde doğrudan kullanımda olmasa da C, C++, C#, Java gibi evrendeki bilgisayarların büyük çoğunluğunu yöneten "C-Ailesi" dillerinin kod mimarisindeki DNA'sı (Yorum satırları, döngüler, blok ve parantez yapıları) doğrudan BCPL'den mirastır.
