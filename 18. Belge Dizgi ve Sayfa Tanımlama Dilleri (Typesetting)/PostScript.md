# PostScript

## Özet
PostScript (PS); 1982 Yılında Adobe (John Warnock ve Charles Geschke) tarafından icat edilen, Yazıcıların (Printer) ve Ekranların Ekrana Çizecekleri Lekemsi veya Çizgisel Karakterleri Piksel piksel iletmek(Bitmap Eziyeti) yerine; **"Vektörel (Matematiksel Koordinatlarla) Çizim Komutları"** taşıyan ve aynı zamanda Yığın-Tabanlı (Stack-Based), Turing-Complelete Mutlak ve Unutulmuş Devasa bir **Hesaplama ve Sayfa Açıklama (Page Description) Dilidir**. 

(Bilgisayar Dünyasının En Hayati Kalidir. Bugün Kullandığınız PDF (Portable Document Format) Dosyaları bile Aslında Derlenmiş (Sınırlandırılmış ve Donurulmuş) Bir PostScript Varisidir).

## Nedir ve Ne İşe Yarar?
1980 öncesi Bilgisayarınızdan Bir Cıktı (Print) almak isterseniz, Bilgisayarınız Yazıcıya "Şu pikseli siyah boya, şunu beyaz yap" diye Trilyonlarca Nokta Atar/Yollardı(Dot Matrix - Nokta Vuruşlu Eziyetleri). Eğer O resmi büyütürseniz, Kare Kare (Minecraft gibi) bozulurdu.

Adobe Dediki: **Yazıcının İçine Bir Bilgisayar Motoru (PostScript Yorumlayıcısı) Koyacağım!**
Bilgisayardan Çıktı(Print) Emri Verdiğinizde Kablodan Resim İnmeyecektir. Kablodan Bir POSTSCRIPT PROGRAM KODU Aşağıya (Yazıcıya) inecektir. 
Kod Der Ki : `300 400 moveto (Ahmet) show`. Yazıcının içindeki Motor Bu Kod satırını Alır, 300 ve 400. Koordinata Gider Mükemmel Ve asla Bozulmayan Pürüzsüz "Ahmet" Kelimesini Matematiksel Olarak Çizer. Siz Bu Cıktıyı İsterseniz Bir A4 Kağıda, İhtiyac Olursa Koca Stadyumdaki Devasa Bir Bayraga Bastırın; Matematiksel Eğriler (Bézier Curves) sayeseinde ASLA kalite Düşmez!

**Ne İşe Yarar?**
* **Masaüstü Yayıncılık Devremi (DTP):** Bugün eğer Mükkeml Basılan Dergilersiniz, T-Shirt çizimlerniz ve Apple(Mac) ile İletişim kuran Dev yazıcılar varsa Bütün Endüstri PostScript ve O vektörek (Kayıpsız / Ölçeklenir(Scalable Font) ) Sihir sayesindedir.
* Font (Yazı Tipi) Evreni: (.ttf Veya .otf İsimli Windows Font dosyalarının Arkasındaki O kavisleri sağlayan Motor).

## Dilin Mantığı ve Kod Yapısı
Evrenin en Garip Dili (Seviye 2'deki Forth Dilinin Torunudfur)! Çünük **Yığın (Stack Tabanlı)** Dır! Tıpkı Bir Silahın şarjõrü gibi Tabağa Veriler Atılıe (Push), Sonra Fonksiyon Yazılır ve Alttaki Verileri Ezip Yok eder (Pop). Çarğırma Şekili Sondan başa Doğru(Reverse Polish Notation) çalışır.

Turing Tam bir Eziyet Dili olduğu İçin, PDF belgesinin arkasında Siz İsterseniz "Fractal Cizen, Yada Asal Sayı Bulan" Makro/Loop algoritması yzacabilitsziniz!!!

### Örnek Bir PostScript Kodu: Bir Kutucuk Çizip, İçine Dünyaya Selam (Hello World) Yazan O Dosya
Eğer Alttaki Kodu Yazar ve Bir Lazer Yazıcıya(Eski tarz) Atarsanız, Yazıcı Sayfanın Ortasında Güzel Bir Dikdötegn İçnde Bize Seslenir:

```postscript
%!PS   % En Ustte Bunun Bir Postcript Dosysasi Olddugynu Belirtiirz (Sihir)

% 1. YAZI TIPI VE BOYUTU AYARLAMA MANTIFI (Son Ekten / Sona(Stack))

/Helvetica findfont      % Helvetica Fontunu BUL (Tabağa/Stack'e Koy)
20 scalefont             % Bu fontu 20 Puntoya Ölçekle (Ayarla)
setfont                  % Ve Bunu Geçerli Kalem/Yazi yap (Aktar!)

% 2. RENK AYARLAMA  (RGB Formatinda 0-1 Arası, Örnek: Kırmızıya boyayalim)
1 0 0 setrgbcolor        % Kırmzıyı 1 yap(Al), Yesil 0, Mavi 0.

% 3. CIZME / HARAKET MANTIGI
200 400 moveto           % X(200), Y(400) kordinatina Git (Kalemi Kagitta oraya KOY!)
(Merhaba Dunya!) show    % Ekrana Bu String Purenzle / Vur !!

% 4. ALTINA BIR UFAK CIZGI (Kutu / Line) CIZELIM
100 380 moveto           % Yeni Koordinata Git!
300 380 lineto           % Oraya Kadar bir Düz Cizgiiii ÇİZ (Line to)
5 setlinewidth           % Cizginin Kalınlıği(Ucu) 5 Olur
0 0 1 setrgbcolor        % Kalemin rengi Mavi Olut
stroke                   % Ve Karala (Uygyla/Boya)!!

% 5. SAYFAYI YAZICIDAN DISARI FIRLAT !! 
showpage                 % YAZICI MOTARUNA (Benim Kodlamam Bitti Yazdir(Printle)) DiR!

```
Görüldüğü gibi PostScript dizekliği, Normal bir Yazılımda `DrawLine(200,400)` Şeklinde (Argüman parantezli) Değildir! Ters Polish(RPN) Mantığıdır!  Yani Önce Argümanlar verilir: `200 400 .. MauseOrayaGit`.
Adamlar Matbaacılıkı ve Resmi %100 "Yazılım Tarafından Çizilecek Matematiğe" oturtmuş Dünyanın En efsane Sırlarından biridir.

## Kimler Kullanır?
* Cıktı Sektörü: Dünyadaki Matbacılar ve Eski (Ama güçlü) Illustrator Tasarımcıların Kaynak Kodları ve Illustrator (AI) dosyeları Arka planda EPS (Encapsulated PostScript) Taşıyıcılardr.
* Bütün Evren Kullanır: Siz Bilgisayaryanızda herhangi bir .PDF Belgesi Okuduğunuzda O Dosyanımn İçi aslında Bu dili Kullanan Güvenliğii Dondurulmuş, Silahsızlandırıılmş Binary/Makrodur. Modern Dünyada PDF Her Yerde Olduğu İuin Adobe (Sanatkar) Firması ve Apple Bu dil üzerinden 30 Yıldır Milyar Dolarlık İmpatorluk kurmuştur.
