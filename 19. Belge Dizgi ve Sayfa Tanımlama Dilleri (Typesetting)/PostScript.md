# PostScript

## Özet
PostScript (PS); 1982 Yılında Adobe (John Warnock ve Charles Geschke) tarafından icat edilen, Yazıcıların (Printer) ve Ekranların Ekrana Çizecekleri lekevari veya Çizgisel Karakterleri Piksel piksel iletmek (Bitmap Eziyeti) yerine; **"Vektörel (Matematiksel Koordinatlarla) Çizim Komutları"** taşıyan ve aynı zamanda Yığın-Tabanlı (Stack-Based), Turing-Complete Mutlak ve Unutulmuş Devasa bir **Hesaplama ve Sayfa Açıklama (Page Description) Hayati Dilidir**. 

(Bilgisayar Dünyasının En Hayati Dilidir. Bugün Kullandığınız PDF (Portable Document Format) Dosyaları bile Aslında Derlenmiş (Sınırlandırılmış ve Dondurulmuş) Bir PostScript Varisidir).

## Nedir ve Ne İşe Yarar?
1980 öncesi Bilgisayarınızdan Bir Çıktı (Print) almak isterseniz, Bilgisayarınız Yazıcıya "Şu pikseli siyah boya, şunu beyaz yap" diye Trilyonlarca Nokta Atar/Yollardı(Dot Matrix - Nokta Vuruşlu Eziyetleri). Eğer O resmi büyütürseniz, Kare Kare (Minecraft gibi) bozulurdu.

Adobe Dedi ki: **Yazıcının İçine Bir Bilgisayar Motoru (PostScript Yorumlayıcısı) Koyacağım!**
Bilgisayardan Çıktı (Print) Emri Verdiğinizde Kablodan Resim İnmeyecektir. Kablodan Bir POSTSCRIPT PROGRAM KODU Aşağıya (Yazıcıya) inecektir. 
Kod Der Ki : `300 400 moveto (Ahmet) show`. Yazıcının içindeki Motor Bu Kod satırını Alır, 300 ve 400. Koordinata Gider Mükemmel Ve asla Bozulmayan Pürüzsüz "Ahmet" Kelimesini Matematiksel Olarak Çizer. Siz Bu Çıktıyı İsterseniz Bir A4 Kağıda, İhtiyaç Olursa Koca Stadyumdaki Devasa Bir Bayrağa Bastırın; Matematiksel Eğriler (Bézier Curves) sayesinde ASLA kalite Düşmez!

**Ne İşe Yarar?**
* **Masaüstü Yayıncılık Devrimi (DTP):** Bugün eğer Mükemmel Basılan Dergiler, T-Shirt çizimleriniz ve Apple (Mac) ile İletişim kuran Dev yazıcılar varsa Bütün Endüstri PostScript ve O vektörel (Kayıpsız / Ölçeklenebilir (Scalable Font)) Sihir sayesindedir.
* Font (Yazı Tipi) Evreni: (.ttf Veya .otf İsimli Windows Font dosyalarının Arkasındaki O kavisleri sağlayan Motor).

## Dilin Mantığı ve Kod Yapısı
Evrenin en Garip Dili (Seviye 2'deki Forth Dilinin Torunudur)! Çünkü **Yığın (Stack Tabanlı)** Dır! Tıpkı Bir Silahın şarjörü gibi Tabağa Veriler Atılır (Push), Sonra Fonksiyon Yazılır ve Alttaki Verileri Ezip Yok eder (Pop). Çağırma Şekli Sondan başa Doğru(Reverse Polish Notation) çalışır.

Turing Tam bir Eziyet Dili olduğu İçin, PDF belgesinin arkasında Siz İsterseniz "Fractal Çizen, Yada Asal Sayı Bulan" Makro/Loop algoritması yazabilirsiniz!!!

### Örnek Bir PostScript Kodu: Bir Kutucuk Çizip, İçine Dünyaya Selam (Hello World) Yazan O Dosya
Eğer Alttaki Kodu Yazar ve Bir Lazer Yazıcıya (Eski tarz) Atarsanız, Yazıcı Sayfanın Ortasında Güzel Bir Dikdörtgen İçinde Bize Seslenir:

```postscript
%!PS   % En Ustte Bunun Bir PostScript Dosyası Olduğunu Belirtiriz (Sihir)

% 1. YAZI TIPI VE BOYUTU AYARLAMA MANTIĞI (Son Ekten / Sona(Stack))

/Helvetica findfont      % Helvetica Fontunu BUL (Tabağa/Stack'e Koy)
20 scalefont             % Bu fontu 20 Puntoya Ölçekle (Ayarla)
setfont                  % Ve Bunu Geçerli Kalem/Yazi yap (Aktar!)

% 2. RENK AYARLAMA (RGB Formatında 0-1 Arası, Örnek: Kırmızıya boyayalım)
1 0 0 setrgbcolor        % Kırmızıyı 1 yap(Al), Yeşil 0, Mavi 0.

% 3. ÇİZME / HAREKET MANTIĞI
200 400 moveto           % X(200), Y(400) koordinatına Git (Kalemi kağıtta oraya KOY!)
(Merhaba Dunya!) show    % Ekrana Bu String Pürüzsüz / Vur !!

% 4. ALTINA BİR UFAK ÇİZGİ (Kutu / Line) ÇİZELİM
100 380 moveto           % Yeni Koordinata Git!
300 380 lineto           % Oraya Kadar bir Düz çizgi ÇİZ (Line to)
5 setlinewidth           % Çizginin Kalınlığı (Ucu) 5 Olur
0 0 1 setrgbcolor        % Kalemin rengi Mavi Olur
stroke                   % Ve Karala (Uygula/Boya)!!

% 5. SAYFAYI YAZICIDAN DIŞARI FIRLAT !! 
showpage                 % YAZICI MOTORUNA (Benim Kodlamam Bitti Yazdır (Printle)) Der!

```
Görüldüğü gibi PostScript dizekliği, Normal bir Yazılımda `DrawLine(200,400)` Şeklinde (Argüman parantezli) Değildir! Ters Polish (RPN) Mantığıdır! Yani Önce Argümanlar verilir: `200 400 .. MouseOrayaGit`.
Adamlar Matbaacılığı ve Resmi %100 "Yazılım Tarafından Çizilecek Matematiğe" oturtmuş Dünyanın En efsane Sırlarından biridir.

## Kimler Kullanır?
* Çıktı Sektörü: Dünyadaki Matbaacılar ve Eski (Ama güçlü) Illustrator Tasarımcıların Kaynak Kodları ve Illustrator (AI) dosyaları Arka planda EPS (Encapsulated PostScript) Taşıyıcılardır.
* Bütün Evren Kullanır: Siz Bilgisayarınızda herhangi bir .PDF Belgesi Okuduğunuzda O Dosyanın İçi aslında Bu dili Kullanan Güvenliği Dondurulmuş, Silahsızlandırılmış Binary/Makrodur. Modern Dünyada PDF Her Yerde Olduğu İçin Adobe (Sanatkar) Firması ve Apple Bu dil üzerinden 30 Yıldır Milyar Dolarlık İmparatorluk kurmuştur.
