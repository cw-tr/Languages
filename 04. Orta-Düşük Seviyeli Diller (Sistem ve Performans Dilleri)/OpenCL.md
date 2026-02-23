# OpenCL

## Özet
OpenCL (Open Computing Language); NVIDIA'nın "sadece benim donanımımla çalışır" dediği kapalı kaynaklı dev CUDA teknolojisine başkaldırı olarak Apple himayesinde ortaya çıkmış (ve Khronos grubuna bağışlanmış), Ekran Kartları (GPU) başta olmak üzere sistemdeki her türlü çok-çekirdekli donanımda çalışabilen **Evrensel Paralel Hesaplama** dilidir.

## Nedir ve Ne İşe Yarar?
Eğer süper hesaplama (yapay zeka, 3D Render vb.) yapmak istiyorsanız ve elinizde NVIDIA GPU yoksa (Örneğin Apple'ın Mac M1/M2/M3 çipleri, Sony PlayStation'un AMD GPU'su, ya da standart bir Intel Dahili Grafik yongası varsa) o sistemde **CUDA çalışmaz**. Ne yazık ki donanım kilitlidir.

İşte tam burada OpenCL devreye girer. Bir standart (API spesifikasyonu ve dil hibriti) olarak OpenCL, ister Intel işlemciniz olsun ister oyun konsollarınızdaki AMD GPU olsun; kodunuzun sistemdeki **erişilebilecek bütün paralel işlemcilerde (CPU/GPU/FPGA ayrımı yapmaksızın)** aynı anda asimetrik çalışmasını sağlar (Heterojen Hesaplama).

**Ne İşe Yarar?**
* **Cihazlardan Bağımsız (Hardware Agnostic) Hesaplama:** Yazılan bir OpenCL kodu hem akıllı telefonun ARM çipinde, hem masaüstü AMD Radeon ekran kartında, hem de bir yapay zeka sunucusunda aynen derlenir ve paralel çalışır.
* **Gömülü ve Mobil Ekosistemler:** Mobil cihazlarda CUDA olmadığı için, mobil oyunların ışın izleme (Ray-tracing) simülasyonları ve kameralardaki canlı-yüz filtreleme maskeleri OpenCL destekli donanım ivmelendirmeleriyle yapılır.

## Dilin Mantığı ve Kod Yapısı
OpenCL tıpkı rakibi CUDA gibi, standart *C (ve C++)* dili üzerine inşa edilmiştir (C99 standartlarına uyumludur). Programlama yapan kişinin iki yapıyı kodlaması gerekir:

1. **Host Programı (Konak):** Sistem RAM'inde çalışan ve ekran kartına (veya hedefe) emri/veriyi hazırlayan C/C++ programı.
2. **Kernel Programı (Çekirdek - Device Code):** Yalnızca o cihazın (GPU'nun) içerisinde yüzlerce işçiye bölünerek çalışacak ana "Paralel C Kodu".

En büyük teknik fark, *Çalışma Zamanında Derleme (JIT Compilation)* yapmasıdır. CUDA kodları `nvcc` ile sisteme gömülürken hazır makine (PTX) formatına getirilmiştir; OpenCL ise kernel kodunu tıpkı standart bir "Düz Metin (String)" gibi okuyup, o saniye bilgisayardaki mevcut AMD veya Intel donanımının driver (sürücü) özelliklerine göre sıfırdan derleyip karta yükler. Bu da ona devasa bir cihaz taşınabilirliği verir.

**Örnek İşleyiş (Sembolik Olarak):**
Yine CUDA örneğindeki 1 milyonluk vektör toplamasını yapalım. OpenCL'de Host (Bilgisayar tarafı) bir C dili dizesine String formatında `A = B + C` yazar. Bunu API üzerinden ekran kartına iletir. Sürücü o andaki karta göre binlerce çekirdeğe `get_global_id` vererek şimşek hızında işi donanıma dağıtır.

### Örnek Bir OpenCL Kodu (Basit Vektör Kernel'ı)
Bir C (Host) programından grafik (veya FPGA) kartına düz metin olarak (String/Dosya) gönderilen ve orada paralel işlenecek OpenCL (Kernel) parçasının ham ve asıl kod hali. (CUDA'daki `threadID` mantığının buradaki karşılığı `get_global_id`'dir).

```c
// Vektörleri toplayacak olan ve GPU donanımında paralel olarak asimetrik koşturulacak C dili kodu 
// "kernel" olarak belirtilmiştir, Düz C derleyicisi (GCC vb.) bunu anlamaz, OpenCL derleyicisi işler.
__kernel void vector_add(__global const int *A, __global const int *B, __global int *C) {
    
    // OpenCL sihirli komutu: get_global_id(0)
    // GPU'da aynı saniyede uyanan 10.000 işlemciden her biri,
    // kendi eşsiz "işçi indeksini(id)" buradan öğrenir. Biri 50 olur, diğeri 51.
    int id = get_global_id(0);

    // Döngü yok! Her işçi doğrudan kendisine ait sıradaki işi halleder ve biter.
    C[id] = A[id] + B[id];
}
```

Bu kernel dizisi, ana C kodunuz çalıştırıldığı esnada API üzerinden ekran kartına yüklenip eşzamanlı olarak patlatılır. (İki taraflı yazılması gerektiği için CUDA'ya (Sadece <<< >>> yazıp geçmeye) kıyasla çok daha meşakkatli, "Boilerplate/Tekrarlayan" bir geliştirme süreci vardır.)

## Kimler Kullanır?
* NVIDIA kilitlenmesinden kurtulup AMD kartlarında Kripto (Ethereum zamanlarında) madenciliği yapmak üzere kazım motoru (Miner) geliştirenler.
* Apple ve Adobe gibi donanım yelpazesi çok geniş olan büyük platformlar (Photoshop, Premiere içerisindeki ağır Mercury Playback grafik efekti render hesaplama modülleri uzun yıllar OpenCL kullandı).
* Akıllı cihaz, oyun konsolları (PS/Xbox) ve FPGA (Gömülü çip) tabanlı Yapay Zeka/Görüntü İşleme mühendisleri. 
