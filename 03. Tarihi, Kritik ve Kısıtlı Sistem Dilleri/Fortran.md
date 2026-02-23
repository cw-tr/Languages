# Fortran

## Özet
Fortran (Formula Translation); 1957 yılında IBM tarafından icat edilen, dünyanın yaşayan **en eski genel amaçlı ve derlenen programlama dilidir**. Günümüzde dahi özellikle süper bilgisayarlarda yüksek performanslı matematiksel ve bilimsel hesaplamalarda yeri doldurulamaz bir devdir.

## Nedir ve Ne İşe Yarar?
1950'lerde bilim insanları, meşakkatli ve hata yapmaya çok açık olan Assembly diliyle kod yazmaktan yorulmuşlardı. Fortran, doğrudan insanların okuyabildiği ve daha sonra bilgisayarın kendi kendine Makine Diline çevirdiği (Derlediği) dünyadaki ilk dil olma unvanını taşır.

Yıllar içinde (Fortran 77, 90, 2018 gibi) modernleşerek prosedürel ve esnek bir hal alsa da, en baştan itibaren devasa matrisleri, vektör işlemlerini ve çoklu dizileri saniyenin milyarda biri hızlarda çarpmak/bölmek amacıyla yazılmıştır.

**Ne İşe Yarar?**
* **Ağır Matematik ve Simülasyon:** Kasırgaların ve iklimin hareketlerini saat saat tahminleyen hava durumu radarları, deprem modellemeleri, ve kıtalararası füzelerin balistik yörünge simülasyonları Fortran ile yürütülür. 
* **Süper Bilgisayarlar:** C/C++ dilleri pointer (işaretçi) aritmetiğinde bellek parçalanmasına yol açabilirken, Fortran derleyicileri dev dizilerin bellekte kesin sınırlarla durduğunu bilir; bu sayede dünyadaki en güçlü süper-bilgisayarlar olan (HPC) sistemlerinde (Örn: TOP500 listesi) işlemleri en üst seviyede paralelleştirip inanılmaz saf optimize edilmiş makine kodu üretir.

## Dilin Mantığı ve Kod Yapısı
Fortran, tamamen diziler (arrays) ve matematiksel denklemler etrafında şekillenmiştir. Bilim insanları ve fizikçiler rahat etsin diye, matematiksel formüllere en yakın olan doğal sözdizimi seçilmiştir. 

Güncel sürümlerinde C veya Python gibi dillerde alışkın olduğumuz modüller, paralel hesaplama komutları (Do Concurrent) yapısı yerleşik halde gelir. Hafıza yönetimi ve karmaşık pointer işlemleriyle uğraşmadan, bilimsel formülü doğrudan yazmaya odaklanılır.

**Örnek İşleyiş (Sembolik Olarak):**
C++ ile 100 milyon elemanlı iki devasa matrisi (tabloyu) birbiriyle toplamak için içi içe döngüler ("for x, for y") kurup bilgisayarı meşgul edersiniz. Fortran'da ise doğrudan `C = A + B` demeniz yeterlidir, Fortran derleyicisi bu iki dev matrisi arka planda binlerce çekirdeğe gönderip saniyeler içinde doğal olarak çarpar-toplar. Matematik işlemi dildeki yerel (native) emirdir.

### Örnek Bir Fortran Kodu: Matris İşlemi
1'den 10'a kadar olan sayılarla ilgilenen ufak bir bilimsel hesaplama fonksiyonu:

```fortran
! Modern Fortran'da (!) yorum satırıdır.
program VektorHesaplama
    implicit none
    
    ! 5 elemanlı Reel (Ondalıklı) sayı dizileri (Vektörler) tanımlıyoruz
    real, dimension(5) :: ruzgarHizi
    real, dimension(5) :: havaBasinci
    real, dimension(5) :: toplamEnerji
    integer :: i

    ! Vektörlerin içini bazı fiziksel değerlerle dolduruyoruz
    ruzgarHizi = [12.5, 14.2, 11.0, 9.8, 15.6]
    havaBasinci = [1013.2, 1010.5, 1015.0, 1008.3, 1005.1]
    
    ! FORTRAN'IN GÜCÜ: İki dev diziyi tek bir matematik komutuyla çarpar ve atar.
    ! Döngü vs (for, while) kurmaya gerek yoktur.
    toplamEnerji = ruzgarHizi * havaBasinci

    ! Sonuçları ekrana yazdırmak için standart bir döngü
    print *, "Analiz Sonuçları:"
    do i = 1, 5
        print *, "İstasyon", i, " Değer:", toplamEnerji(i)
    end do
    
end program VektorHesaplama
```

Modern Fortran derleyicisi, `toplamEnerji = ruzgarHizi * havaBasinci` satırını görür görmez sistemin işlemcisindeki (CPU) SIMD (Tek Komut, Çoklu Veri) uzantılarını aktive ederek inanılmaz yüksek bir hızda paralel işleme başlar.

## Kimler Kullanır?
* Fizikçiler, Meteorologlar (Hava tahmin ajansları, NOAA), ve İklim Analistleri.
* Nükleer santrallerin reaksiyonlarını modelleyen mühendisler, çarpışma testleri ve mekanik hesaplamalar yapan akademisyen ve bilim insanları.
* Üniversitelerin akademik "Yüksek Başarımlı Hesaplama (HPC)" merkezlerindeki araştırmacılar.
