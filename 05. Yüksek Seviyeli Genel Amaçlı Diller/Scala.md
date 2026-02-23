# Scala

## Özet
Scala (Scalable Language); 2004 yılında geliştirilen, Java Sanal Makinesi (JVM) üzerinde çalışan, tıpkı C++'ın makine ve donanım arasında köprü kurması gibi, evrendeki en zor iki programlama tarzını (**Kesin Nesne Yönelimli OOP** ile **Saf Fonksiyonel Programlamayı**) birbiriyle kusursuzca kaynaştırmak üzere yaratılmış çok devasa ve karmaşık bir Büyük Veri (Big Data) dilidir.

## Nedir ve Ne İşe Yarar?
Eğer bir uygulama saniyede milyarlarca tweet veya banka işlem verisi alıyorsa, klasik (Stateful - değişkenlerin durmadan yeni rakamla güncellendiği) programlama (Java, C#) feci bir tıkanmaya sebep olur. "Fonksiyonel Programlamada" o anki veri (örneğin X=5 ise) **asla değişmez** (Immutability). Sayı değişecekse orijinal X kalır, yepyeni bir Y(6) yaratılır. Bu da milyarlarca verinin "Aman aynı anda iki kişi aynı veriye yazmaya kalktı silecek!" derdi (Data-Race) olmadan eşzamanlı trilyonlarca hesaplama sunmasını sağlar.

Scala, devasa şirketlere şunu vadetti: "Java'nın güçlü Class/Objelerine ve muazzam altyapısına dokunmayın o kalsın. Ama üzerine fonksiyonel dilin saf matematiksel veri akışını ekleyelim, sistemler sonsuz BÜYÜYEBİLSİN (Scalable)".

**Ne İşe Yarar?**
* **Büyük Veri Analizi (Big Data) ve Akan Veri (Streaming):** Evrendeki dev "Saniyede Akan" veriler (Twitter'ın zaman tünelindeki anlık akan trilyonlarca tweet, Netflix metrikleri, IoT sensör okyanusları) **Apache Spark** veya **Apache Kafka** adlı efsanevi açık kaynaklı motorlarla işlenir. Bu motorların %90'ı Scala ile yazılmıştır.
* **Dağıtık ve Eşzamanlı (Distributed) Sistemler:** "Akka" aktör mimarisi sayesinde, büyük bir bankacılık veya telekom firması, sunucularından 5 bin tanesinin aynı saniyede farklı milyonlarca faturayı birbirine karışmadan çarpıp bölmesi gerektiğinde kodları çökmeden Scala altyapısından dağıtır.

## Dilin Mantığı ve Kod Yapısı
Sloganları "Sade ama Çok Güçlü (ve biraz zor öğrenilen)" sözdizimidir. Tıpkı Kotlin gibi Scala da Java kütüphanelerini dilediği gibi sömürebilir. 

Scala'da her şey, ama **her şey** istisnasız bir Objedir (Functions are also Objects). Java tipi `int` (ilkel/primitive) bir tiptir, Scala'da ise O da bir sınıftır. En kritik felsefesi verinin değiştirilemez (Immutable) olmasıdır. Bir kez tanımlandıktan sonra sabit kalması tavsiye edilir (`var` yerine hep `val` kodlanması), böylece yan-etkisi (Side-effect) olmayan matematiksel, öngörülebilir güvenli veri nehirleri yaratılır.

**Örnek İşleyiş (Sembolik Olarak):**
Java'da binlerce kişilik listeyi gezip (for) yaşı otuzdan büyük olanları bulmak için if-else kodlamak en az 10 satır sürer. Scala ise veri kümelerini "Map, FlatMap, Filter" gibi matematiksel işlevlerle su borusu gibi birbirine bağlar. `Liste.filtrele(30dan buyukleri).donustur(isimleri)` demek tüm işi 1 satırda halleder. Derleyici arka planda eşzamanlılığı dağıtır.

### Örnek Bir Scala Kodu: Fonksiyonel Veri Ayıklama
Dev bir veri listesindeki yapıları klasik döngülerle ezmek yerine *Gelişmiş Desen Eşleştirme (Pattern Matching)* ve *Matematiksel Fonksiyonlarla Dağıtma* işlemlerinin Java'nın devasa hantal kod yapısını nasıl kısmen Lisp/Haskell-vari bir güzelliğe çevirdiğini görelim:

```scala
// Scala'da tekil (Statik) uygulama baslangiclari icin "object" kavrami kullanilir. Singelton pattern'dir.
object VeriMadencisi {

  // Değiştirilemez (Immutable) bir Tip/Veri yapısı tanımlayalım: (Kotlin'deki data class gibidir)
  // "case class" arkada yüzlerce satır kodu şablonlayan fonksiyonel programlama sihridir
  case class Musteri(isim: String, yasi: Int, alisverisTutari: Double)

  def main(args: Array[String]): Unit = {

    // Bankadan akan çok büyük ve "Sabit/Değişmeyen (val)" milyonlarca veriden ufak bir Liste kesiti:
    val musteriler = List(
      Musteri("Ahmet", 25, 1500.50),
      Musteri("Ayşe", 32, 24000.00),
      Musteri("Zeynep", 19, 450.00),
      Musteri("Mehmet", 45, 12000.75)
    )

    // İŞTE SCALA FARKI (Fonksiyonel Veri Zinciri - Pure Functions):
    // Klasik dongu felsefesi YOKTUR. Filtrele (Filter) ve Üzerinden Geç (Map) vardir!
    // Sadece yasi 30'un altindaki, ve Harcamasi 1000 uzerindekileri Cek, İsimlerini Alip Listele:
    
    val gencVeZenginMusteriler = musteriler
      .filter(m => m.yasi < 30 && m.alisverisTutari > 1000.0) // İstenmeyenleri ELEDİK
      .map(m => m.isim)                                       // ELİMİZDE KALANLARIN SADECE ismini kopardık
      
    // Geriye tertemiz küçük listeyi bırakır (Immutable olduğu için asıl Listemiz Asla Bozulamadı!).
    
    println("--- Pazarlama Hedef Kitlesi ---")
    
    // Gelişmiş FOR kurgusu: Listeyi okutalım
    for (hedef <- gencVeZenginMusteriler) {
      println(s"Kampanya E-Postasi Gonderilecek: $hedef")
    }
  }
}
```
Yukarıdaki veri işleme hızı ve kısalığı (Scala'nın mucidi bu matematiksel zincirlemelerdir), Twitter ve Netflix gibi şirketleri kilitlenmelerden kurtarıp asenkron rahatlığa kavuşturmuştur.

## Kimler Kullanır?
* Evrendeki Büyük Veri (Big Data) Mühendisleri. Saniyelik verilerin Petabaytları aştığı ve analiz edilmesinin saniyeler içerisinde bitmesi gereken Hadoop, Apache Spark altyapılarında işlem yapan veri mimarları.
* Java dünyasında kalmak isteyen, kütüphanesini değiştiremeyen ama çok hızlı ölçeklenmek (Scale) ve fonksiyonel programlama yapmak isteyen Finans, Telekom ve E-Ticaret devlerindeki (Enterprise) ileri düzey (Senior) yazılım mühendisleri. 
* *Dezavantajı:* Aşırı karmaşık ve matematiksel kurallar içerdiği için giriş bariyeri (öğrenme süresi) en zor dillerden biridir.
