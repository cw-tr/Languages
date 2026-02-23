# Kotlin

## Özet
Kotlin; 2011 yılında JetBrains (IntelliJ IDEA'nın yaratıcıları) tarafından geliştirilen, eskiyen ve hantallaşan Java'nın tahtını alarak **Android dünyasının yeni ve tek resmi dili** ilan edilmiş, %100 Java (JVM) uyumlu, inanılmaz modern, kısa ve "Pragmatik" bir programlama dilidir.

## Nedir ve Ne İşe Yarar?
Yıllarca Java ile Android veya Sunucu programlayan şirketler, Java'nın "aşırı kelime kalabalığından" (Boilerplate) bıkmıştı. Ufacık bir sınıfı yaratmak için onlarca satır Getter/Setter, Constructor, Exception bloğu yazmak gerekiyordu. 

JetBrains mühendisleri, "Biz Java Sanal Makinesi (JVM) üzerinde çalışacak, eski devasa Java kütüphanelerinin hepsini sıfır hatayla doğrudan import edebilen (Interoperability), ama sözdizimi olarak Swift veya C# kadar kısa, şık ve güvenli yepyeni bir dil yapıyoruz" dediler. Kısa sürede Android dünyasını fethettiler.

**Ne İşe Yarar?**
* **Modern Android Geliştirme (Mobile):** 2017'de Google, Kotlin'i Android için birinci sınıf (first-class) diller arasına aldığını duyurdu; 2019'da ise Java'yı resmi olarak terk edip Kotlin'i **Tek Önerilen Dil** yaptı. Bugün tüm modern Android uygulamaları ve bankacılık/sosyal medya mobil sistemleri bu dille yazılır.
* **Modern Backend (Sunucu) Sistemleri:** Java'nın devasa ekosistemini (Örn: Spring Boot) milisaniyede içe aktarabildiği için, firmalar var olan arka plan sunucularını sıfırdan yazmadan sadece yeni özellikleri Kotlin ile kısacık yazıp Java projesinin içine gömebilmektedir.
* **Coroutines (Eşzamanlılık):** Tıpkı Go (Golang) dilinin başardığı "Milyonlarca hafif işi sisteme yük bindirmeden dondurarak (suspend) yönetme" mucizesini, kendi "Coroutine" mantığı ile JVM dünyasına da taşıyan dildir.

## Dilin Mantığı ve Kod Yapısı
Felsefesinde **"Null Güvenliği (Null Safety)"** yatar. Tıpkı Swift dilindeki mucizevi (?) operatörü gibi Kotlin'de de bir veri yoksa sistem "Null Pointer Exception" diyerek çökmez. Milyar dolarlık hatayı bitirmiştir.

İkinci büyük felsefesi **Eziyet Veren Söz Dizimini Kısaltmaktır.** Java'da 60 satır süren "Data Class" tanımları Kotlin'de tek satırdır. Noktalı virgül (`;`) yoktur. Değişkenleri yaratırken tipini uzun uzun yazmazsınız (`val x = 5` yeterlidir). `new` kelimesine ihtiyaç yoktur. 

**Örnek İşleyiş (Sembolik Olarak):**
Java'da "Kullanıcı" adında bir objeyi ve ona ait İsim/Yaş/Şifre bilgilerini saklamak, ekrana bastırmak için satırlarca kod yazarsınız. Kotlin'de ise başlık açar gibi tek bir parantez içi kod yazarsınız; derleyici (compiler) arka planda yüzlerce satır Java Bytecode'unu sizin için 1 saniyede otomatik üretir!

### Örnek Bir Kotlin Kodu: Data Class ve Null Kalkanı
Kotlin'in neden Java'nın tahtını bu kadar hızlı aldığını kanıtlayan, hem "Data Sınıfı" hem de boş (Null) verilere karşı güvenli harika bir Android/Backend tarzı model örneği görelim:

```kotlin
// JAVA'DA 80 SATIR SÜRECEK KOD BURADA TEK SATIR: (Data Classes)
// Bu "data" kelimesi, arkada otomatik olarak eşleştirme, Hash, kopyalama
// ve Java'nın Getter/Setter komutlarının HEPSİNİ yaratir!
data class UyeModeli(val uyeAdi: String, var yas: Int, var premiumMu: Boolean? = null)

// "val" (Value) : Sabit veridir. Sonradan DEGISTIRILEMEZ. Mükemmel Güvenliktir.
// "var" (Variable) : C'deki standart değişkendir. Sonradan değişebilir.
// "Boolean?" : Sonundaki soru isareti, bu verinin ici BOŞ (NULL) gelebilir, hazirlikli ol demektir!

fun main() {
    
    // Obje üretirken C# veya Java'daki gibi "new" kullanma eziyeti kalkmıştır.
    val yeniUye = UyeModeli("Serkan", 28) 

    // Premium bilgisi üstte girilmedi (Varsayılan null oldu).
    // Güvenlik Zinciri (Safe Call Operator `?.` veya Elvis Operator `?:`)
    
    // Asagidaki satirda eger uye null degilse yazdir, 
    // ama '?:' ile "EĞER null ise su yedek metni bas" dileriz ve PROGRAM ASLA DONMAZ/ÇÖKMEZ.
    
    val uyelikDurumu = yeniUye.premiumMu ?: "Kullanicinin Uyelik Paketi Belli Degil (NULL Gecildi)!"
    
    println("Yeni Gelen Uye: ${yeniUye.uyeAdi}") 
    println("Durum: $uyelikDurumu") 
    
    // String birlesimi (Interpolation), "Kullanici: " + isim + " falan" demekten kurtarir.
    // Doğrudan cümlenin içine ${degisken} gomuyoruz.

    if (yeniUye.yas >= 18) {
        // Kotlin'de (List) veya Arrayler çok hızlı yaratılır. Map(), Filter() her şey C# Linq gibidir.
        val hosgeldinRozetleri = listOf("Baslangic", "Gece Kusu") 
        println("Sahip Oldugu Rozetler: $hosgeldinRozetleri")
    }
}
```
Yukarıdaki kısacık ve temiz kod yüzünden Android Developer dünyası Java dilini tamamen geride bırakmış, Google da bunu standart bir kural olarak tasdiklemiştir.

## Kimler Kullanır?
* Milyarlarca cihaza inen oyun, e-ticaret ve medya Uygulamalarını baştan aşağı Android Studio'da tasarlayan Front-end Mobil Geliştiricileri.
* Hem Backend(Spring) hem Frontend hem de iOS yazılım işini (Kotlin Multiplatform Mobile - KMM platformu ile) TEK BİR DİLDE yazarak saatlerce vakit tasarrufu etmek isteyen tam yığın (Full-stack) şirket mimarları.
* Java'nın ağır Enterprise (Kurumsal Veritabanı) projelerine yeni modern arayüzler katan Java-Kotlin melez entegratörleri.
