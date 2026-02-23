# MATLAB

## Özet
MATLAB (Matrix Laboratory); 1970'lerin sonlarında Fortran tabanlı doğrusal cebir kütüphanelerine (LINPACK ve EISPACK) kolayca erişebilmek için bir üniversite hocası olan Cleve Moler tarafından icat edilmiş, bugün The MathWorks şirketi tarafından geliştirilen **tamamen ticari (kapalı kaynak/ücretli)**, dünyanın en yaygın Akademik ve Mühendislik simülasyon/matris programlama dilidir.

## Nedir ve Ne İşe Yarar?
1970'li yıllarda mühendislik veya fizik okuyan bir öğrencinin, 100x100'lük devasa bir çarpım matrisi hesaplamak için Fortran dilinde değişkenleri deklare etmesi, pointer taklaları atması ve bilgisayarı derlemek için saatlerce beklemesi gerekiyordu. 

Cleve Moler, öğrencilerin bu Fortran eziyetini çekmemesi için sadece bir "Hesap Makinesi" gibi çalışacak arayüzlü bir dil yarattı. MATLAB, adından da anlaşılacağı gibi **Matris (Matrix) odaklıdır**. Dildeki her şey, ekrana `A = 5` yazsanız bile, aslında arkada "5 sayısını barındıran 1'e 1'lik dev bir matris (Array)" olarak algılanır.

**Ne İşe Yarar?**
* **Mühendislik Simülasyonları ve Simulink:** Sadece kod değil bir ekosistemdir. Dünyadaki tüm uçak (Boeing), otomotiv (Ford, Toyota) fren merkezleri ve güç aktarım kontrol mekanizmaları MATLAB ile test edilir. Özellikle içindeki "Simulink" adlı grafiksel blok diyagram aracı sayesinde kod yazmadan, kutuları birbirine bağlayarak motorun ısısı denklemlerle modellenir.
* **Sinyal ve Görüntü İşleme:** Sağlık (MR Cihazı, EKG Dalgası), Askeri Radar (Sinyal/Spektrum analizi) ve Haberleşme sektörü, çok yoğun sinyal filtrelemelerini MATLAB kütüphanelerinin eşsiz güvencesiyle çözer. Zira parayla satılan kapalı bir sistem olduğu için matematiksel bir hataya (Örn. Yanlış filtreleme) şirket sıfır tolerans garantisi verir.

## Dilin Mantığı ve Kod Yapısı
Nesne yönelimi veya Sistem Programlamasıyla (Bir Oyun, bir Mobil Uygulama vb.) uzaktan yakından alakası yoktur. Bir Algoritma Laboratuvarı'dır. 

Bütün akıl "Döngüleri çöpe atıp MATRİSLERİ birbiriyle doğrudan çarpmak (Vektörizasyon)" üzerine kuruludur. Lineer cebirdeki kurallar geçerlidir (Örn: Çarpım operasyonunda matrisin tersini almak (`inv()`) ve denklem çözmek `\` gibi sembolik kısaltmalara sahiptir). 

Tiplendirme tamamen zımni (Implicit) çalışır, tip vermeniz gerekmez. Rengi olan, pencereler açan, üç boyutlu grafikleri (Eğri, Tepeler - Meshgrid) kod biter bitmez kendi içinde fırlatan devasa grafik motorlarına da native olarak sahiptir.

**Örnek İşleyiş (Sembolik Olarak):**
Python'da İki listeyi direkt çarparsanız (Eğer Numpy kurmadıysanız) Hata fırlar ya da Listeyi ucuca ekler. MATLAB'de ise "Sinyal x Sinyal2" yazdığınız an doğrusal cebir kurallarıyla mükemmelen skaler veya matrisyel dev sonuçlar (Türev ve İntegralleri dahil) sistemin önbelleğinde beliriverir.

### Örnek Bir MATLAB Kodu: Sinyal / Frekans Optimizasyonu
Klasik bir yazılımcının "Bu ne biçim değişken atamak" dediği ama Elektronik/Makina mühendisliği dillerinde "Tahtaya denklem çizer gibi" sayılan sinyal/grafik simulasyon örneği:

```matlab
% MATLAB'da efsanevi sekilde yorum satirlari '%' (Yuzde) ile başlar.
% Zaman sinyali (Vektoru) yaratiyoruz:
% Mantik su -> (Baslangic: 0.01 adimlarla : Bitis 10 saniye araliginda yuzlerce sayi)
zaman = 0:0.01:10; 

% Bir sinus ve ikinci bir hizli sinus dalgasini matris seklinde ust uste bindirip ÇAKALIM:
hedef_sinyal = sin(2 * pi * 1.5 * zaman) + 0.5 * sin(2 * pi * 5 * zaman);

% Eger hedefe (Gurultu / Noise) eklemek istersek Random/Rastgele Matrix uretelim:
% "randn(size(zaman))" zaman vektoru ile ayni uzunlukta parazit uretir.
gurultu_sinyali = hedef_sinyal + 0.3 * randn(size(zaman));


% === MATEMATİKSİZ SADECE YAPAY ZEKAYI CAGRİMA GUCU ===
% MATLAB'in kendi gömülü pürüzsüzleştirme "smooth" (Filtreleme) fonksiyonu asil algoritma gizleyicisidir.
temizlenmis_sinyal = smooth(gurultu_sinyali, 20); 

% === EKRANA DEVASA GRAFIKLERI BASMA (FIGURE) ===
figure; % Ekranda yep yeni bir Windows Grafik Penceresi aç
hold on; % Ayni pencerenin ustune "katmanlari" kapatmadan çiz

% 1. Cizgi (X ve Y ekseni, sadece verileri ver formata gore Kirmizi (r) ile parazit ciz):
plot(zaman, gurultu_sinyali, 'r', 'DisplayName', 'Gürültülü(Bozuk) Sinyal');

% 2. Cizgi (Veriyi filtreli olanla Mavi (b), Çizgi kalinligi(LineWidth) 2 olacak sekilde Çiz)
plot(zaman, temizlenmis_sinyal, 'b', 'LineWidth', 2, 'DisplayName', 'Filtrelenmiş Ses Sinyali');

% Grafigin Isim ve etiket ayarlarini firlat (Yine tek satirla):
xlabel('Gercek Zaman (s)');
ylabel('Dalga Genligi / Frekans (Hz)');
title('Radar Sinyal Filtreleme Simülasyonu');
legend('show'); 
grid on; % Izgarayi ac
```

Normalde bu pencere grafiğini açıp bu veriyi C dilinde işlemek aylar sürerdi.

## Kimler Kullanır?
* Hemen hemen evrendeki her (**Makine Mühendisliği ve Elektronik - Telekomünikasyon Mühendisliği**) öğrencisi ve akademisyeni (İlgili üniversiteler pahalı lisanslarını satın alarak donanımlı mezunlar verirler).
* Otomotiv devleri, Askeri Radar sistemlerinde roket kontrol yörüngesi mimarisindeki algoritmaları gömülü cihazlara indirmeden hemen önce sanal ortamda simüle eden Kontrol Sistemi (Control Systems) analistleri.
* Ciddi rakipleri **Python** (SciPy/NumPy ekosistemi ve ücretsiz olması yüzünden çok müşteri kaptırmıştır) ve **Julia** dilleridir. Yine de sektör standartlarıyla dolu kapalı kutu güvenirliği ile yerini hâlâ büyük oranda korur.
