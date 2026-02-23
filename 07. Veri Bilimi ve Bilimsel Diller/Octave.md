# Octave

## Özet
GNU Octave (Octave); 1988'de Kimyasal Mühendislik asistanı John W. Eaton tarafından yaratılan, dünyanın en devasa ve pahalı lisanslı matematik platformu olan **"MATLAB" dilinin, %100'e yakın tam uyumluluk (Syntax Uyumu) ile bedava, Açık Kaynak Topluluğu (GNU) tarafından klonlanmış** muazzam Matrix çözümleyici ve matematiksel modelleme dilidir.

## Nedir ve Ne İşe Yarar?
1990 ve 2000'li yıllarda mühendislik bölümleri okuyan (Makine, Elektrik-Elektronik vb) tüm bilim insenları Sinyal modellemeleri, dev matris tersleme işlemleri yapmak zorundaydı. Bütün donanım dünyasının formülü "MATLAB" diliyle yazılırdı. Ancak MATLAB, şirketler ve zengin ABD okulları için binlerce dolara satılıyordu. Fakir bir öğrenci, ya da Pakistan/Hindistan/Türkiye'deki bir doktora öğrencisi evinde bu mühendislik ödevini yapamıyordu.

Octave şövalye gibi devreye girdi: "Ben ücretsiz ve özgür yazılım olarak, sizin yazdığınız o pahalı MATLAB (.m) uzantılı kodlarınızın kopyasını okuyacağım, derleyeceğim ve kendi bedava penceremden(GUI) Çizimini yapacağım" diyerek doğdu. Octave, matematiksel matris (Array/Matrix) operasyonlarındaki dev Açık Kaynak "Laboratuvarıdır".

**Ne İşe Yarar?**
* **Akademi Mülteciliği / Ücretsiz MATLAB:** Öğrenciler bitirme projelerini, "Numerical Methods (Sayısal Yöntemler)" derslerini MATLAB lisans bedeli ödemeden saf GNU Octave indirerek tıkır tıkır yaparlar. Bir kod MATLAB'de nasıl yazılıyorsa %95 Oranında hiçbir şey değiştirmeden Octave'a yapıştırılır ve aynen çalışır!
* **Makine Öğreniminin (Machine Learning) Orijinal Eğitim Dili:** Meşhur Stanford profesörü Andrew Ng, dünyayı sarsan ve AI patlamasını fitilleyen (Coursera) ilk "Makine Öğrenimi Kursu"nda; Linear Regresyon gradyanlarını (Gradient Descent) öğretirken Python değil, bedava ve matrikslerle dans eden **Octave** dilini zorunlu kılmıştır (10 yıl boyunca AI temelleri hep bu dille öğretilmiştir).

## Dilin Mantığı ve Kod Yapısı
Tamamen (Tıpkı MATLAB gibi) **Vektörizasyon (Vectorization)** üzerine kuruludur. İki boyutlu (Matrix) C dili spagetti döngülerine For(For()) savaş ilan eder! Eğer 1000 elemanlı bir liste varsa onu döngüyle çekmez; dümdüz $A * B$ yazarak Linear Cebir (Linear Algebra - BLAS / LAPACK) C-kütüphanelerini C'den milyon kat daha pürüssüz bir cümleyle çözer.

Sözdizimi çok basittir, Noktalı virgül (`;`) eklerseniz ekrana yazıyı fırlatmaz (Suppress), eklemezseniz o hesabı ekranda "Ans=" diye gösterir. Octave, ekstra olarak C/C++ dillerine daha fazla saygı duyar, C tarzı komentar `//` ve farklılılık `!=` leri de kabul ederek aslında MATLAB'den bile daha esnek bir sözdizimi barındırır arka planda.

**Örnek İşleyiş (Sembolik Olarak):**
Java (For ile iki Arrayı toplatmak).
Octave'da 100 Elemanlı Vektörü kalesini (Sinüsü) almak: `sin(Vektor) .^ 2` (Ortadaki O koca nokta `.` işareti demek; "Eleman eleman (Element-wise) çarp! Matriks çarpımı yapma! Element element git!") demektir.

### Örnek Bir Octave (veya MATLAB) Kodu: Yapay Zekayı ve Türev Formülünü (Regresyon) Döngüsüz Vurmak!
Makine öğreniminin o Matrix/Ağırlık güncellemelerini For döngüsü kullanmadan sadece Matematik (Cebir) Sembolizmi ile milisaniyerde GPU/CPU vuran o efsane yapı:

```octave
% Octave / MATLAB dilinde Yorum Satirlari Yuzde Isareti (%) ile baslar.
% (Ayrica Octave # işaretini de -Python gibi- kabul eder, MATLAB etmez).

% ===============================================================
% 1. VEKTOR VE MATRİX(DIZI) YARATIMI (Cebir Felsefesi)
% ===============================================================

% X Matrisi: 3x3 Bir Kutu. Sayilar Boşlukla Kolon ayrilir, Noktali Virgul(;) ile Alt Satira duser!
X = [1 2 3 ; 4 5 6 ; 7 8 9];

% Sonu (') olan isaret "Devrigi/Transpozunu(T)" al demektir! 
% Yani X matrisini Havada Tersine cevir Matrix Matematiği yap!
X_Transpoz = X';

% Eğer (;) koydugumuz icin ekrana basmayacak. 
% Biz basmasini istiyorsan, komut sonunda noktalı virgul YOK:
disp("X'in Tersine Donmus(Transpoze Edilmis) Hali:");
X_Transpoz   % < Ekrana Şak diye Cikar!


% ===============================================================
% 2. DÖNGÜSÜZ (VECTORIZED) HESAPLAMA MUCİZESİ
% ===============================================================

% 1 ile 10 arasinda Adim Adim (0.5 artisla) giden devasa bir Dizi(Tensor) yarat:
zaman = 1 : 0.5 : 10; 

% "Dongu (For) YASAK!"
% Gidip zaman degiskeninin icindeki yuzlerce elemanin tek satirda Sinusunu(sin) bul
% Ve icini 5 ile carp!. 
% Noktali (^2) '.^' : Cizgisel Matris carpimi(A*B) degil, Her kordinati kendi icinde uzeri Karele(Karesini Al):
sonucEgrisi = sin(zaman) .^ 2;


% ===============================================================
% 3. CIZDIRME (PLOT) VE GORSELLESTIRME
% ===============================================================

% Veriyi Ekrana(Grafik Penceresinde) Aninda Çiz(Plot)! 
plot(zaman, sonucEgrisi, '-ro', 'LineWidth', 2); % Kirmizi (r), Yuvarlak noktali o) ciz.

% Excel Eziyeti Yok! Grafige Title Ekle:
title('Zaman vs Sonuc Eğrisi');
xlabel('Zaman (Saniye)');
ylabel('Dalga Enerjisi');
grid on; % Grafigin arkasina Karekod zemin cizgisi cek!
```
Siz "Run" dediğiniz an, sistemin içinde "Figure 1" diye minik bağımsız harika bir Görüntü Ekranı çıkar ve Linear Cebirin mühendislik hesaplarıyla kucaklanmış 2D renkli grafik raporunuz anında belilir! Kanser hücre tespitinden radar sinyali işlemeye kadar her yeri bu kod döker.

## Kimler Kullanır?
* MATLAB'e lisans parası vermek istemeyip lisans/yüksek lisans ödevlerini GNU açık kaynak ile yapmak isteyen Milyonlarca **Bilim adamı ve Mühendislik(Elektrik, Makine, İnsaat) Öğrencisi**.
* Derin Öğrenmenin "Arkada yatan Matrix Matematiği kanunlarını" Kara Kutu (Pytorch / Tensorflow) modülleriyle değil; elleriyle Linear Cebiri hesaplamak isteyen Coursera ve MIT Veri Bilimi başlangıç öğrenicileri. Lakin Yapay Zeka sektörü %95 oranında Python ekosistemine geçmiş olduğundan günümüzde popülerliği tamamen akademide kalmıştır.
