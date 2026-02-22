# CUDA

## Özet
CUDA (Compute Unified Device Architecture), NVDIA tarafından yaratılmış olan, dünyanın ilk C tabanlı ve doğrudan Ekran Kartı (GPU) çekirdeklerini programlamaya yarayan devrimsel **Paralel Hesaplama** platformu ve yazılım dilidir.

## Nedir ve Ne İşe Yarar?
2000'li yıllara kadar bilgisayar programları sadece Merkezi İşlem Birimi (CPU) denilen 4 veya 8 çekirdekli beyinler üzerinde çalıştırılabiliyordu. Ancak grafik işlemleri için üretilmiş devasa ekran kartları (GPU) o sırada binlerce "küçük ve aptal" çekirdek barındırıyordu. 

2006'da NVIDIA bu devasa atıl gücü serbest bıraktı. CUDA ile yazılımcılara, ekran kartının bu binlerce ufak çekirdeğine "Aynı anda tek bir matematiksel problemi verin ve bin parçaya bölüp anında çözsünler" (General-Purpose computing on GPUs - GPGPU) imkânı tanıdı.

**Ne İşe Yarar?**
* **Yapay Zeka (AI) ve Derin Öğrenme:** ChatGPT'nin eğitildiği o binlerce A100/H100 ekran kartının dilinin alt yapısında saf CUDA yatar. Matris çarpımları gibi ağır AI hesaplamalarını, işlemcide (CPU) aylarca sürecekken, GPU'da (CUDA ile) saniyelere indirger.
* **Kripto Para Madenciliği (Mining):** Bitcoin ve Ethereum algoritmalarını kırmak/hesaplamak için yıllarca tüm dünyada ekran kartlarıyla (GPU) CUDA çekirdekleri kullanılarak madencilik yapıldı.
* **Bilimsel Simülasyon ve Render:** Hava durumu ve DNA dizilim simülasyonları gibi aynı anda 100.000 farklı noktanın analiz edildiği işlemler ile gişe rekorları kıran 3D CGI filmlerinin Render süreçleri bu teknoloji sayesindedir.

## Dilin Mantığı ve Kod Yapısı
Aslında CUDA yepyeni fantastik bir sözdizimi değildir. Tamamen C/C++ dillerinin üzerine (eklenti gibi) giydirilmiş özel komutlardan ve derleyici direktiflerinden oluşur. `nvcc` adlı NVIDIA derleyicisi tarafından derlenir.

Yazılımcı, kodunu normal bir C dosyası gibi yazar ancak, Ekran Kartında çalışmasını istediği ağır hesaplama fonksiyonunun başına özel bir kelime ekler: `__global__` (Global Kernel Fonction).

**Örnek İşleyiş (Sembolik Olarak):**
Elinizde 1 Milyon elemanlı iki dev dizi (Vektör) var ve bunları toplayacaksınız: `A + B = C`. C veya Java dili olsaydı `for` döngüsü 1 milyondan 0'a inene kadar beklerdiniz (1 Tekil çekirdek). CUDA ile ekran kartına "1 Milyon tane farklı işçi çekirdek(Thread) uyanın! Herkes 1 tane indeks toplasın!" dersiniz. Döngü anında **tek saatte** saniyeler sürmeden (Tek Komut, Çok Veri - SIMD) hesaplanıp size geri döner.

### Örnek Bir CUDA Kodu: GPU'da Paralel Vektör Toplama
Standart bir CPU döngüsü değil, her bir GPU (Ekran Kartı) çekirdeğinin tek bir indeks değerine odaklandığı paralel fonksiyon yapısı (`id` hesabına odaklanın):

```cpp
#include <stdio.h>
#include <cuda_runtime.h> // CUDA Kütüphanesi

// DİKKAT: __global__ bu fonksiyonun Ekran Kartının (GPU) içinde Milyon Kere çalışacağını söyler
// Bu fonksiyon "Kernel" diye adlandırılır.
__global__ void vektorTopla(int *dev_A, int *dev_B, int *dev_C, int N) {

    // EKRAN KARTI SİHRİ: Döngü YOKTUR.
    // Her çekirdek (Thread) uyandığı an sadece 'kendi kimlik numarasını(id)' hesaplar.
    // Örneğin 500. çekirdek sadece 500. endeksi işler.
    int tr_ID = blockIdx.x * blockDim.x + threadIdx.x; 

    // Eğer kendi indeksi dizinin limitlerindeyse hesaplar ve yatar:
    if (tr_ID < N) {
        dev_C[tr_ID] = dev_A[tr_ID] + dev_B[tr_ID];
    }
}

int main(void) {
    int N = 1000000; // 1 Milyon eleman sayısı

    /* 1. Ekran kartının VRAM'inde (Gigabaytlık yer) yer aç (cudaMalloc) */
    /* 2. Sistem RAM'inden Ekran kartının VRAM'ine devasa diziyi kopyala (cudaMemcpy) */
    
    // CPU'DAN EKRAN KARTINA GÖNDERME KOMUTU MİMARİSİ:
    // Bu satır C fonksiyonu DEĞİLDİR! Cuda'ya özel Grid/Block hesaplatmasıdır.
    // Diyor ki: GPU üzerinde 256 tane ayrı Çalışma Bloğu uyanıp içindeki işçileri paralel seferber etsin.
    int blocksPerGrid = (N + 255) / 256;
    
    vektorTopla<<<blocksPerGrid, 256>>>(d_A, d_B, d_C, N);

    /* 3. VRAM'deki sonucu ("cudaMemcpy" ile) Sistem RAM'ine geri çek ve ekrana bas. */
    /* 4. VRAM'i temizle ("cudaFree"). */
    return 0;
}
```

Bu kod GPU (Nvidia) üzerindeki binlerce küçük "işçinin" heykeli aynı anda farklı noktalarından kesmesi ile eşdeğerdir.

## Kimler Kullanır?
* Evrendeki tüm Makine Öğrenimi, LLM (Büyük Dil Modelleri) ve Derin Öğrenme Araştırmacıları (Python ile çağırdıkları TensorFlow veya PyTorch'un tabanındaki tüm matematik bu dille çalışır).
* Biyoinformatikçiler (Hücre katlanması ve RNA devasa hesapları) ile Kriptoloji Mühendisleri.
* Veri Analistleri ve Veritabanı hızlandırması amaçlayan Süper Bilgisayar Akademisyenleri.
* **Dezavantaj:** Sadece **NVIDIA** markalı ekran kartlarında ve donanımlarında kapalı kutu (Closed Ecosytem) olarak çalışır. Rakipler (Örn: AMD GPU) bunu çalıştıramaz.
