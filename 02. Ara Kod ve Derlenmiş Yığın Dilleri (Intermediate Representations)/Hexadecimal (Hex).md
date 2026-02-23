# Hexadecimal (Hex Kodları)

## Özet
Hexadecimal (On altılık sayı sistemi); aslında bir programlama dili değil, bir **"Sayı Gösterim Sistemi" ve Makine Dilinin İnsan Sınırlarına Çevrilmiş Yüzüdür**. 0 ve 1'lerden (Binary) oluşan sonsuz ve okunması imkansız olan Makine Kodunun (Machine Code) veya Bellek (RAM) Adreslerinin, `0-9` rakamları ve `A-F` harfleri kullanılarak (16'lık tabanda) sıkıştırılıp "Hex Düzenleyicilerde (Hex Editor)" Mükemmel bir matris boyutunda okunmasını ve hacklenmesini sağlayan Yarı-Dildir.

## Nedir ve Ne İşe Yarar?
Bilgisayar sadece 0 ve 1'i anlar (Binary). Örneğin `A` harfi `01000001` demektir.
Eğer bir "Uygulama.exe" veya "Resim.jpg" dosyasının İÇİNDEKİ kodlara (Binary veriye) bakmak isterseniz ve ekrana 0 ile 1'leri basarsanız; Gözleriniz kanar. 8 haneli o `01000001` kodunu akılda tutmak imkansızdır!

İşte Matematik ve Bilgisayar Bilimi dedi ki: **İkilik Sistemi (Base-2) Onaltılık Sisteme (Base-16) Yuvarlayalım!**
Her 4 adet (0 ve 1) biti, Tek Bir HEX karakterine denk gelir!
- `0000` = `0`
- `1010` = `A` (On)
- `1111` = `F` (On Beş)
Böylece o uzun `01000001` (A Harfi) Binary Kodu ekranda Büyüyle Sıkışarak `41` şeklini (Hex) alır!

Yazılımcılar `41 42 43` gördüklerinde bunun `ABC` olduğunu anlarlar. Dosyaların içine Sızıp (Reverse Engineering) bu Hex'leri Düzenlemek Mümkündür.

**Ne İşe Yarar?**
* **Tersine Mühendislik (Reverse Engineering) & Hack / Modding:** Paralı (Lisanslı) eski bir oyunun EXE dosyasını Hex Editor (Hxd) ile açarsınız. Ekranda binlerce `FF 00 1A 2B` akar. Eğer oyunun Parola Kontrol Kısmındaki `74` (Eşitse Zıpla - JZ) Assembly komutunu fareyle silip yerine `EB` (Her Türlü Zıpla - JMP) Hex'ini Elinizle daktilo ederseniz: **Program Hacklenmiş ve Cracklenmiş olur!**
* **Bellek Adresleri (C/C++ Pointerları):** C Dilinde "Ahu" değişkeninin Bilgisayarın Ram'inde nerede durduğuna baktığınızda `0x7FFE0A5B` gibi bir adres çıkar. O "0x" Evrensel bir Damgadır; "Dikkat! Yanımdaki Numara Onluk değil, HEX(16'lık) sistemdedir!" anlamına gelir.

## Dilin Mantığı ve Kod Yapısı
Tamamen Tablo tabanlıdır. Dosyalar açıldığında Sol tarafta Dosyanın İçindeki Konum (Offset), Ortada "Hex Kodları (Makine Dili)", Sağda ise onun "Müşteri Tarafındaki Metin Karşılığı (ASCII)" görünür.

### Örnek Bir Hex Düzenlemesi (Bir PNG Resminin Gizli İmzası!):
Eğer bir Resim dosyasının (foto.png) "Uzantısını yanlışlıkla .mp3 yapsanız bile", Bilgisayar Düzgün açar! Neden?
Çünkü bilgisayarlar Dosya Uzantısını (.png) UMURSAMAZ. Onlar İçindeki İLK HEX KELİMESİNE (Magic Number / Sihirli Baytlar) Bakar!

Örneğin Bir PNG Resminin Hex Editor'de İlk Başlangıcı Her Zaman Şöyledir:
```text
Offset(Adres) |  Hex (Makine/16lik)      | Metin Çevirisi (ASCII)
00000000      | 89 50 4E 47 0D 0A 1A 0A  | ‰PNG....
00000008      | 00 00 00 0D 49 48 44 52  | ....IHDR
```
En baştaki o `89 50 4E 47` Sabittir (Magic Number). `50 4E 47` = `PNG` Harflerine Tekabül eder. 
Eğer Siz Gidip O `89`'u `FF` Yaparsanız (Hex Editörle) Dosya Bozulur (Corrupt) Ve AÇILMAZ! Virus analistleri şüpheli bir EXE yi açtıklarında İçindeki "MZ" (4D 5A) Başlığına bakarlar (Tüm Windows Exelerin Kralı Mark Zbikowski'nin adıdır). 

## Kimler Kullanır?
* C/C++'taki Segmentasyon Hatalarını(Core Dump) okurken, Belleğin tam o anki fotoğrafını (Hex Dump) tarayarak Hangi Satırda Patlandığını Bulan **Sistem/Çekirdek (Kernel) Hata Ayıklayıcıları (Debuggers)**.
* Oyun Hileleri(Cheats / Cheat Engine) yazan veya Kapalı Dosya formatlarını Parçalayarak "Bunun İçinde Ne Gizlenmiş (Adli Bilişim / Forensics)" diye arayan **Siber Güvenlik Uzmanları ve Modderler**. Düşük seviyenin Saf Kan(Çiğ) Suyudur.
