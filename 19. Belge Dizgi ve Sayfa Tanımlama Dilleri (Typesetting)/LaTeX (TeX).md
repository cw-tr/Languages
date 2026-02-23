# LaTeX (TeX)

## Özet
LaTeX; Aslen 1978 yılında Üniversite Profesörü ve Algoritma Babası **Donald Knuth** tarafından (Matematik kitaplarının yayıncılardaki çirkin ve iğrenç dizgisinden nefret ederek) icat ettiği "TeX" motorunun üzerine, 1984 Yılında Leslie Lamport tarafından İnşa edilmiş Devasa bir **Belge Dizgi (Typesetting) ve İşaretleme (Markup) Sistemidir**. Dünya Üzerindeki BÜTÜN Akademik makaleler, Doktora Tezleri ve Ağır Matematik kitapları Word ile değil, Kusursuz Çıktı veren bu Kodlama ile hazırlanır!

## Nedir ve Ne İşe Yarar?
1970'lerde bir bilgisayar Bilimcisiyseniz ve Kitabınıza "İntegral Sınırları içerisinde Karekök Alfa" Formülünü basmak istiyorsanız; Dizgiciler (Basımevleri) o Sembolleri Daktiloda Yan Yana koyamıyor O Yüzden Bütün Matematiği Yamuk Yumuk Çirkin şekilde Kağıda basıyorlardı. (Donald Knuth The Art of Computer Programming kitabıda bunu yaşadı).

Knuth 10 yıl Odaya kapanıp, Kağıdın üzerindeki Milyarlık piksellere "Fırça vuran (Hangi harfin hangi harfle kaç milim mesafesi (kerning) olacağının formülünü yazan) TeX dilini çıkarttı.
Bugün Bir Word (Microsoft) dosyasında Formül yazarken veya İçindekiler Tablosu kaydığında Sinir Krizi geçirirsiniz (WYSIWYG - Ekranda Ne Çizersen O Mantığı).
LaTeX Der ki: Fareyi Bırak! Kod Yazarak Kitap Çıkaracaksın (Declaration)! "Burası Başlık De, Sayfa Numarasını 2 satır C++ kılıklı Yazı yaz Ben Onu MATBAADA MÜKEMMEL Pdf olarak Bastırayım!".

**Ne İşe Yarar?**
* **Akademi ve Bilimsel Çıktı (Makale/Kağıt):** Üniversitelerin Fizik, Matematik, Bilgisayar Mühendisliği Bölümlerindeki Doktora tezlerinin Yüzde Yüzü Bu dil ile Yazılmak Zorundadır, Çünkü Otomatik Referanslama/Kaynakça Sistemi kusursuzdur.
* **Tipografik Kusursuzluk:** Bir kelimenin Alt Satıra geçmesi (Tireleme/Hyphenation) gerektiğinde, Word Cümleyi Kötü böler ve Boşluk atar. Lakin LaTeX O paragrafın İçindeki Bütün kelimelerin Alanlarını (Penalty-Score) Yapay zeka Olasılığıyla tarayıp Sayfayı "Matbaadan çıkmış Muazzam bir Asaletle" çizer.

## Dilin Mantığı ve Kod Yapısı
Tamamen **Ters-Taksim (Backslash - `\`)** ve Kıvırcık Parantezler `{}` üzerine Kurulu Özel, Karmaşık bir Metin biçimlendirme mimarisidir. 
Programlama dili değildir lakin İçinde Döngüler Makrolar ve Atamalar olduğu için Turing-Complete (Tam) bir Motor Özelliği Taşır!

### Örnek Bir LaTeX Kodu: Karmaşık Matematiksel Formülü Olan Bir Sayfa Çizmek!
Word'de yazarken Kilitleneceğiniz O Çetrefilli Türev/İntegral Denklemeli Fizik Sayfasını, Terminalde daktilo ederek PDF'e Çeviren O Mükemmel Kodlama:

```latex
% BU BİR LATEX DOSYASIDIR (Yuzdeyuz İhtişam!) Ve Yorum Satiri Yuzde İsaretidir.

% 1. KAĞIT ÜZERİ ÖZELLİKLERİ VERİYORUZ (Word'deki Yeni Belge Tuşu):
\documentclass[12pt, a4paper]{article}

% Kullanacağımız Matematik Kütüphanelerini Çekiyoruz!
\usepackage{amsmath}
\usepackage{graphicx}

% Belgenin Kapağındaki Yazar İsimleri!
\title{Einstein'ın Genel Görelilik Kodlaması}
\author{Doktor Ali Veli}
\date{\today}  % Otomatik Bugünün saatini Vuracak!

% ================= BELGE MİMARISI BASLIYOR (BODY) =================
\begin{document}

% Kapak Sayfasini Otomatik Olustur (Yukaridaki Bilgileri Kullanarak)!
\maketitle

\section{Giriş ve Kainat}
Bu çalışmada, evrendeki kütleçekimi sadece Newton eziyeti ile değil, 
modern tensör hesabı ile aşağıdaki gibi göstereceğiz. (Bu paragraf sayfa boşluğuna göre harika dizilecek!)

\subsection{Alan Denklemleri}
Aşağıdaki formülü Microsoft Word'de yazamazsınız, ama LaTeX'te saniyedir:

% MATEMATIK FORMÜLÜ (Ekrana Tam Ortala):
\begin{equation}
    R_{\mu\nu} - \frac{1}{2} R g_{\mu\nu} + \Lambda g_{\mu\nu} = \frac{8\pi G}{c^4} T_{\mu\nu}
\end{equation}

Bu formüldeki $\Lambda$ (Lambda) Kozmolojik sabittir ve Albert abinin en büyük vizyonudur. \cite{Einstein1915}

\vspace{1cm} % 1 Santim Boşluk Bırak!!

\textbf{Dikkat:} Burada Yazdığım yazılar kalınlaşacak ama Geri kalan Yazı Normal.

% ================= BELGE BITTI ! =================
\end{document}
```

Bu kod `pdflatex belgem.tex` komutuyla Derlendiğinde (Compile edildiğinde) Ortaya çıkan PDF Dosyası; Sayfa numaraları Eklenmiş, Paragraflar Kusursuz Hizalanmış ve Formül Asla pikselleşmeyen (Vektorel) Ekrana Vurulmuş Klasik Bir Bilimsel (Nature) makalesidir! `\cite` diyerek Atıf yaptığı yazar Otomatik Kaynakça (References) kısmına kendi iner!

## Kimler Kullanır?
* Evrendeki Hemen hemen bütün **Akademisyenler, Profesörler, Matematikçiler**. Zaten arXiv.org sitesinde gördüğünüz tüm fizik makaleleri bu dilde Gönderilir ve Site O Kodlardan PDF üretir.
* Şık (CV) Özgeçmiş hazırlamak ya da belgeler oluşturmak üzere Hazırlanan, "Tasarım Kaymasın, Başlığı nereye atayım" Diye Uğraşmak İstemeyen Düzen hastası (Ayrıca Overleaf web platformunu Kullanan) Zeki Yazılımcılar. Donald Knuth'un Dünyaya Armağan Ettiği Mükemmel Sanattır.
