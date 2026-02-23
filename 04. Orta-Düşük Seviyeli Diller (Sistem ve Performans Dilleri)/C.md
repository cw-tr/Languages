# C

## Özet
C dili, 1972'de Bell Laboratuvarları'nda Dennis Ritchie tarafından yaratılan, günümüz modern bilgisayar biliminin ve neredeyse tüm işletim sistemlerinin üzerine inşa edildiği "programlama dillerinin atası" ve en saf sistem dilidir.

## Nedir ve Ne İşe Yarar?
Makine dili ve Assembly'nin donanım bağımlılığından ve aşırı zorluğundan kurtulmak amacıyla, UNIX işletim sistemini yeniden ve donanımdan bağımsız yazabilmek için B dili üzerinden geliştirilmiştir (Eksik olan Veri Tipleri/Types eklenmiştir). Bütün sistemleri yönetebilmek için donanıma inanılmaz bir yakınlık sağlarken, aynı kodun hem Intel hem ARM çipinde çalışabilmesi (Portability) devrimini getirmiştir.

**Ne İşe Yarar?**
* **İşletim Sistemleri C ile Yazılır:** Windows kernel'ı, Linux çekirdeği, macOS'un (Darwin) kalbi ve Android'in çekirdeği baştan aşağıya devasa C kodlarından oluşur. C olmadan günümüz cihazları açılamaz.
* **Gömülü Sistemler (Embedded):** Mikrodalga fırınınızın içindeki çipten, Mars rotorlarına (Ingenuity), arabanızdaki ABS fren beynine kadar her şey C ile yönlendirilir.
* **Diğer Dillerin Temeli:** Python (CPython), PHP, Ruby, Java Sanal Makinesi (JVM) ve JavaScript motorları (V8) gibi tüm yüksek seviyeli diller aslında arka planda dev bir C programıdır.

## Dilin Mantığı ve Kod Yapısı
C dilinin ana felsefesi programcıya sonsuz güç, ancak "Sıfır Güvenlik ağı" vermektir. Tıpkı BCPL'de olduğu gibi bloklar süslü parantezlerle `{ }` belirlenir, her komut noktalı virgül `;` ile biter. 

En kritik özelliği **Göstericiler (Pointers)** teknolojisidir. Size RAM üzerinde herhangi bir adrese (Örn: `0x7FFF56A4`) doğrudan okuma ve yazma fiziksel iznini verir. Ayrıca bellek bilgisayar tarafından otomatik temizlenmez; `malloc` ile ondan 5 MB RAM istersiniz, sonra işiniz bitince `free` komutuyla o bellek alanını işletim sistemine o blok numarasını bizzat geri iade etmek **zorundasınız**. Eğer unutursanız "Memory Leak" (Bellek Sızıntısı) oluşur ve cihaz kitlenir.

**Örnek İşleyiş (Sembolik Olarak):**
Java veya Python'da `x = 5` dersiniz, değişken sizin için korumalı bir sandığa konur. C dilinde ise `*ptr = 5` yazdığınızda bilgisayara şu emri verirsiniz: "Hafızadaki şu fiziksel koordinata git, oradaki baytları doğrudan yak ve 00000101 yap!".

### Örnek Bir C Kodu: Pointer Görüntüleme
Klasik olarak hafızadan yer isteyip, veri yükleyip o verinin bellek adresini ekrana basan bir çekirdek konsept:

```c
#include <stdio.h>
#include <stdlib.h> // Malloc bellek tahsisi için kütüphane

int main() {
    /* 1. ADIM: İşletim sisteminden 1 tane "tam sayı" sığacak kadar KESİN bellek alanı iste */
    int *gizliAdres = (int*) malloc(sizeof(int));

    if (gizliAdres == NULL) {
        printf("Sistem RAM'de yer vermedi! Çöktü.");
        return 1;
    }

    /* 2. ADIM: O adrese git, içine 42 değerini "manuel" zerk et */
    *gizliAdres = 42;

    /* Ekrana değeri ve RAM'deki gerçek Hexadecimal fiziksel koordinatını bas */
    printf("Deger: %d\n", *gizliAdres);
    printf("RAM'deki Fiziksel Adresi: %p\n", (void*)gizliAdres);

    /* 3. ADIM: Kırmızı Çizgi! İşi biten RAM adresini İşletim Sistemine geri iade et. */
    /* Bu unutulursa ve program döngüdeyse, sistemin tüm RAM'i saniyeler içinde tükenir. */
    free(gizliAdres);

    return 0; // Sorunsuz kapat
}
```

## Kimler Kullanır?
* Linus Torvalds (Linux'un yaratıcısı) gibi Kernel geliştiricileri, Donanım sürücüsü (Driver - .sys uzantılı dosyalar) yazan donanım mühendisleri.
* Veritabanı motorlarını (PostgreSQL, SQLite) sıfırdan inşa eden mimar yazılımcılar. 
* IoT cihazı, drone kartları, Arduino sensör programlayıcıları (Gömülü Yazılım Mühendisleri).
