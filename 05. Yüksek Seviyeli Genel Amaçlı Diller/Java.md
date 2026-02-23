# Java

## Özet
Java, 1995 yılında Sun Microsystems (şu an Oracle) tarafından geliştirilen, "Bir Kere Yaz, Her Yerde Çalıştır" felsefesinin tartışmasız lideri olan, dünyanın en yaygın Nesne Yönelimli (OOP), tam teşekküllü ve kurumsal programlama dilidir.

## Nedir ve Ne İşe Yarar?
90'lı yıllarda yazılımcıların en büyük problemi yazdıkları C veya C++ programını Windows için ayrı, Linux için ayrı, Mac için ayrı derlemek zorunluluğuydu. Çünkü her işletim sistemi donanıma farklı emir veriyordu. Java, **Java Sanal Makinesi (JVM - Java Virtual Machine)** denilen efsanevi motoru yarattı. Java kodunuz doğrudan işlemcinin anlayacağı 0 ve 1'lere derlenmez, sadece JVM'nin anladığı "Bytecode"a derlenir. Bilgisayarınızda (veya buzdolabınızda) JVM kuruluysa, o Java kodu dünyadaki her donanımda milim değiştirmeden aynen çalışır.

**Ne İşe Yarar?**
* **Büyük Kurumsal Sistemler (Enterprise):** Bankacılık altyapıları, E-Ticaret devleri (Amazon vb.), faturalandırma yazılımları, devlet sistemleri (E-Devlet altyapıları) gibi çökmemesi, ağır güvenlik denetimlerinden geçmesi gereken tüm Backend (Sunucu arkası) sistemler on yıllardır devasa Java altyapılarında (Örn: Spring Boot) koşar.
* **Android Dünyası:** Dünyadaki akıllı telefonların %70'ini oluşturan Android işletim sisteminin yerel (doğal) ve kalıtsal programlama dilidir (Her ne kadar yerini resmi olarak Kotlin'e devretse de arka planda her Android cihaz bir Java minyatürüdür).
* **Büyük Veri (Big Data):** Apache Hadoop, Kafka veya Elasticsearch gibi dünyanın petabaytlarca verisini işleyen en devasa arama motorları ve log sunucuları Java dilinde yazılmıştır.

## Dilin Mantığı ve Kod Yapısı
Java tartışmasız bir **Sınıf (Class)** diktatörlüğüdür. Nesne Yönelimli Programlamanın (OOP) en saf halidir. En ufak bir "Ekrana Merhaba yaz" fonksiyonunu bile rastgele boşluğa yazamazsınız, her şey mutlaka ve mutlaka bir Klasın (sınıfın) içine paketlenmek (Encapsulation) zorundadır.

Ondan önceki abilerinin (C/C++) aksine, Java'da programcı bellek adresiyle (pointers) oynayamaz, bellek isteyemez (malloc) veya silemez (free). Her şey otomatik **"Garbage Collector" (Çöp Toplayıcı)**'ın insafındadır. Sizin işiniz biten verileri (objeleri) Java motoru arka planda kendi kendine RAM'den siler.

**Örnek İşleyiş (Sembolik Olarak):**
Java'da "Araba" adında bir Kalıp (Class) çizersiniz. Araba klasının "Motor()", "Direksiyon()" gibi yeteneklerini (Metotlar) belirlersiniz. Sonra bu kalıptan RAM üzerinde `new Araba()` diyerek gerçek objeler oluşturursunuz.

### Örnek Bir Java Kodu: Klasik Zırhlı OOP Yapısı
Bir oyun veya banka hesabındaki "Karakter" ya da "Kullanıcı" profilinin saf nesneye dönüştürülmesi ve ekrana bastırılması (Derleme değil, Bytecode mantığı hakimdir):

```java
// JAVA FARKI: Dosya adı ile ana sınıfın adı (OrnekZirh) TAM AMANLA aynı olmak ZORUNDADIR.
public class OrnekZirh {
    
    // GİZLİ VERİ (Encapsulation): Sadece bu sınıf görebilir (private), 
    // Dışarıdan kimse doğrudan "Banka Hesabim = 5" yazıp hackleyemez.
    private String isim;
    private int kredi;

    // YAPICI(CONSTRUCTOR): Yeni bir nesne yaratıldığında verilecek ilk değerler
    public OrnekZirh(String isimParam, int krediParam) {
        this.isim = isimParam;
        this.kredi = krediParam;
    }

    // AÇIK METOT (public): Dış donanımların ve sınıfların bu nesneyle güvenli iletişim yolu
    public void bilgileriGoster() {
        // "System.out.println", Java'nın meşhur ve uzun konsol yazdırma komutudur
        System.out.println("Kullanici: " + this.isim + " | Bakiye: " + this.kredi);
    }

    // PROGRAMIN BAŞLAMA NOKTASI: C dilindeki main fonksiyonunun Java formatına hapsolmuş hali
    public static void main(String[] args) {
        
        // "new" diyerek yepyeni bir RAM objesi oluşturduk
        // Bu veriyi (objeyi) kim silecekti? Biz değil! Java'nın Çöp Toplayıcısı silecek.
        OrnekZirh kullanici1 = new OrnekZirh("Alice", 1500);
        
        kullanici1.bilgileriGoster();
    }
}
```

## Kimler Kullanır?
* Evrendeki milyonlarca kurumsal Java Backend Dev (Oracle Certified) geliştiricisi ve dev şirketlerin "Enterprise" mimarları. Devlette, E-Nabız'da, Trendyol/Amazon sunucu çekirdeğinde.
* Android Studio'da hayatlarındaki ilk mobil uygulamayı yazan oyun (Örn: Minecraft da ilk olarak tamamen Java ile yazılmıştır) veya araç programcıları.
* Bankaların kritik API'lerini ve mikroservislerini JVM üzerinde orkestra eden DevOps devleri.
