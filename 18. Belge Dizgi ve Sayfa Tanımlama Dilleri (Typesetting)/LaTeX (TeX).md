# LaTeX (TeX)

## Özet
LaTeX; Aslen 1978 yılında Üniversite Profesörü ve Algoritma Babası **Donald Knuth** tarafından (Matematik kitaplarının yayıncılardaki çirkin ve iğrenç dizgisinden Nefret ederek) icat ettiği "TeX" motorunun üzerine, 1984 Yılında Leslie Lamport tarafından İnşa edilmiş Devasa bir **Belge Dizgi (Typesetting) ve İşaretleme (Markup) Sistemidir**. Dünya Üzerindeki BÜTÜN Akademik makaleler, Doktora Tezleri ve Ağır Matematik kitapları Word ile değil, Kusursuz Cıktı veren bu Kodlama ile hazırlanır!

## Nedir ve Ne İşe Yarar?
1970'lerde bir bilgisayar Bilimcisiyseniz ve Kitabınıza "İntegral Sınırları içerisinde Karekök Alfa" Formülünü basmak istiyorsanız; Dizgiciler (Basımevleri) o Sembolleri Daktiloda Yan Yana koyamıyor O Yüzden Bütün Matematiği Yamuk Yumuk Çirkin şekilde Kağıda basıyorlardı. (Donald Knuth The Art of Computer Programming kitabıda bunu yaşadı).

Knuth 10 yıl Odaya kapanıp, Kağıdın üzerindeki Milyarlık piksellere "Fırça vuran (Hangi harfin hangi harfle kaç milim mesafesi(kerning) olacaginin formulunu yzaran) TEX dilini çıkarttı.
Bugün Bir Word (Microsoft) dosyasında Formül yazarken veya İçindekiler Tablosu kaydığında Sinir Krizi geçirirsiniz (WYSIWYG - Ekranda Ne Cizerseb O Mantığı).
LaTeX Derki: Fareyi Bırak! Kod Yazarak Kitap Çıkaracaksın(Declaration)! "Burası Başlık De, Sayfa Numarasını 2 satır C++ kılıklı Yazı yaz Ben Onu MATBAADA MÜKEMMEL Pdf olarak Bastırayım!".

**Ne İşe Yarar?**
* **Akademi ve Bilimsel Çıktı (Makale/Kağıt):** Üniversitelerin Fizik, Matematik, Bilgisayar Mühendisliği Bölümlerindeki Doktora tezlerinin Yüzde Yüzü Bu dil ile Yazılmak Zorunadadır, Çünük Otomatik Referanslama/Kaynakca Sistemi kusursuzdur.
* **Typogfrahic Kusursuzluk:** Bir kelimenin Alt Satıra geçmesi (Tireleme/Hyphenation) gerektiğinde, Word Cümleyi Kötü BÖler ve Boşluk atar. Lakin LaTeX O paragrafın İçindeki Bütün kelimelerin Alanlarını (Penalty-Score) Yapay zeka Olasılığıyla tarayıp Sayfayı "Matbaadan çıkmış Muazzam bir Asaletle" cizer.

## Dilin Mantığı ve Kod Yapısı
Tamamen **Ters-Taksim (Backslash - `\`)** ve Kıvırcık Parantezler `{}` üzerine Kurulu Özel, Karmaşık bir Metin biçimlendirme mimarisidir. 
Programlama dili değilldir lakin İçinde Döngüler Makrolar ve Atamalar olduğu için Turing-Complete(Tam) bir Motor Özelliği Taşır!

### Örnek Bir LaTeX Kodu: Karmaşık Matematiksel Formülü Olan Bir Sayfa Çizmek!
Word'de yazarkan Kilitleneceğiniz O Çetrefilli Türev/İntegral Denklemeli Fizik Sayfasını, Terminalde daktilo ederek PDF'e Ceviren O Mükemmel Kodlama:

```latex
% BU BİR LATEX DOSYASIDIR (Yuzdeyuz İhtisam!) Ve Yorum Satiri Yuzde İsaretidir.

% 1. KAGIT UZERI OZELIKLERİ VERIYORUZ (Word'deki Yeni Belger Tusu):
\documentclass[12pt, a4paper]{article}

% Kullanacagimiz Matematik Kutuphanenlerini Cekiyoruz!
\usepackage{amsmath}
\usepackage{graphicx}

% Belgenin Kapagindaki Yazar İsimleri!
\title{Einstein'in Genel Görelilik Kodlaması}
\author{Doktor Ali Veli}
\date{\today}  % Ototmatik Bugunun SAatini Vuracak!

% ================= BELGE MİMARISI BASLIYOR (BODY) =================
\begin{document}

% Kapak Sayfasini Ototmatiuk Olustur (Yukaridaki Bilgileri Kullanarak)!
\maketitle

\section{Giriş ve Kainat}
Bu çalışmada, evrendeki kütleçekimi sadece Newton eziyeti ile değil, 
modern tensör hesabı ile aşağıdaki gibi göstereceğiz. (Bu paragraf sayfa boslulugan gore harika dizilecek!)

\subsection{Alan Denklemleri}
Aşağıdaki formülü Microsoft Word'de yazamazsınız, ama LaTeX'de saniyedir:

% MATEMATIK FORMÜLÜ (Ekrana Tam Ortala):
\begin{equation}
    R_{\mu\nu} - \frac{1}{2} R g_{\mu\nu} + \Lambda g_{\mu\nu} = \frac{8\pi G}{c^4} T_{\mu\nu}
\end{equation}

Bu formüldeki $\Lambda$ (Lambda) Kozmolojik sabittir ve Albert abinin en büyük vizyonudur. \cite{Einstein1915}

\vspace{1cm} % 1 Santim Bosluk Birak!!

\textbf{Dikkat:} Burada Yazdigim yazilar Kalinlasacak ama Geri kalan Yazi Normal.

% ================= BELGE BITTI ! =================
\end{document}
```

Bu kod `pdflatex belgem.tex` komutuyla Derlendiğinde (Compile edildiğinde) Ortaya çıkan PDF Dosyası; Sayfa numaraları Eklenmiş, Paragraflar Kusursuz Hizalanmış ve Formül Asla piksesllenmeyerin (Vektorel) Ekrana Vurulmuş Klasik Bir Bilimsel (Nature) makalesidir! `\cite` diyerek Atıf yaptığı yazar Otomatik Kaynakça (References) kısmına kendi iner!

## Kimler Kullanır?
* Evrendeki Asılsız bütün **Akademisyenler, Profesörler, Matematikçiler**. Zaten arXiv.org sitesinde gödüğünüz tüm fizik makaleeri bu dilde Gönderilir ve Site O Kodlardan PDF üretir.
* Şık (CV) Özgeçmiş Hatırlamak Üzere Hazırlanan, "Tasarım Kaymasın, Başlığı nereye atayım" Diye Uğraşmak İstemeyen Düzen hastası (Ayrıca Overleaf web platformunu Kullanan) Zeki Yazılımcılar. Donald Knuth'un Dünyaya Armağan Ettiğİ Mükemmel Sanattır.
