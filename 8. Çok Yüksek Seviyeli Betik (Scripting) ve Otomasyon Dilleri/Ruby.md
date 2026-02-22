# Ruby

## Özet
Ruby; 1995 yılında Japon bilgisayar bilimcisi Yukihiro Matsumoto ("Matz") tarafından, yazılım dünyasındaki aşırı karmaşık C ve Perl dillerinin stresini yok etmek üzere icat edilmiş, parolası **"Geliştirici Mutluluğu (Developer Happiness)"** olan, son derece şık, %100 her zerresiyle Nesne Yönelimli (OOP) zarif bir ifade (Betik/Scripting) dilidir.

## Nedir ve Ne İşe Yarar?
1990'larda programcılar "Makineler/Bilgisayarlar işi asıl yapan kölelerdir, programcıların onlar için anlaşılmaz 0/1 makine dillerini, noktalı virgülleri yazarken acı çekmesi tuhaftır. Dil, makineyi değil Programcının psikolojisini mutlu etmelidir" vizyonu ile tamamen insancıl ve yüksek seviyeli mimariyle tasarlandı.

Ancak Ruby asıl şöhretini 2004 yılında, David Heinemeier Hansson'un Ruby üzerine icat ettiği muazzam İnternet Altyapısı (Web Framework) olan **"Ruby on Rails (RoR)"** ile yakaladı. Rails, internet sitesi kodlamak için gerekli (veritabanı, güvenlik, arayüz, url) her şeyi "Convention over Configuration (Ayarlama yapma, benim varsayılan harika kurallarıma uy) mantığıyla saniyeler içinde otomatik olarak kendi çözen büyüsel bir motordu.

**Ne İşe Yarar?**
* **Start-Up Efsanesi (Ruby on Rails):** GitHub, Shopify, Airbnb, Twitch, Twitter (ilk çıkışı) gibi bugün dünyanın en değerli teknoloji firmalarının tamamı ilk (ve birçoğu hala ana) web yapılarını "Aylar süren Java geliştirme süreleri"ni beklemeden, Ruby on Rails'in o "1 Saatte İnternet Sitesinin Backend'i hazır" büyüsüyle canlıya aldılar.
* **Siber Güvenlik Otomasyonu:** Dünyanın en meşhur Açık Kaynak Güvenlik Zafiyeti araştırma/kullanma ve otomasyon sömürü(Exploit) altyapısı olan **Metasploit Framework** tamamen Ruby diliyle yazılmış, siber güvenlikçilerin şaheseridir.

## Dilin Mantığı ve Kod Yapısı
Ruby **HER ŞEY BİR NESNEDİR (Everything is an Object)** ideolojisinin kralıdır. Sadece bir metin veya oluşturduğunuz sınıf değil, dümdüz `5` sayısı kalemi bile bir objedir!

Bir Ruby uygulamasında hiçbir yerde int, float yazmazsınız (Dinamic Typing). Noktalı virgül (`;`) nadiren kullanılır. Satırları bitiren süslü parantezler yerine şık ve konuşkan `end` kelimeleri blok kapatır. Parantez kullanımı metot çağrılarında bile opsiyoneldir (Parantezsiz kullanıp şiir gibi yazmanız alkışlanır). Meta-programlama yeteneği öylesine büyüseldir ki (Örn: metot isimlerini program yayındayken koda yazdırabilirsiniz), RoR'un "Nasıl çalışıyor inanamıyorum (Magic)" hissiyatını buradan gelir.

**Örnek İşleyiş (Sembolik Olarak):**
Örneğin Java veya C#'da ekrana "Merhaba" kelimesini 5 Kere yazdırmak için: `for(int i=0; i<5; i++) { print("Merhaba") }` dersiniz.
Ruby ideolojisi ise bunu bir düz yazıya döker ve "Her şey Objedir" kuralını işletir. Beş sayısı (5) bir Objedir, ona kendi Metodunu ekleyip şunu söylemeniz yeterlidir: `5.times { puts "Merhaba" }`. Bitti! 5 sayısına "kendini 5 kere çalıştır(times)" dedik.

### Örnek Bir Ruby Kodu: "Zarafet" ve İngilizce Okunurluk
Programcıyı mutlu etmek üzerine kurulu olan; Döngülerin, Sayıların ve Sınıfların İngilizce düz bir kitap metni okurmuşcasına tasarlanmış büyüsel nesne kurgusu:

```ruby
# Yorumlar Perl'den ve Bash'tan gelen efsane '#' isaretiyle başlar.
# Devasa sınıflar yerine zarif bloklar ('end' ile biter); Tipler belirtilmez.

class SiparisKutusu
  
  # "attr_accessor" Geliştirici Mutluluğudur. Java'da haftalar süren
  # Get ve Set fonksiyonlarının (Getter/Setter) otomatik Metaprogramlama Makrosudur!
  attr_accessor :urun_ismi, :fiyat, :hazir_mi

  # "initialize", Constructor (Yapılandırıcı) anlamindadir.
  def initialize(isim, fiyat, hazir_durumu = false)
    @urun_ismi = isim # "@" (At Isareti) bunun bir Class_Özelliği (Instance Variabe) oldugunu gosterir
    @fiyat = fiyat
    @hazir_mi = hazir_durumu
  end

  # Soru Isaretli Metodlar (!?): Ruby, eger bir metot sadece "EVET / HAYIR (Boolean)" donecekse 
  # metodun adinin sonuna Soru ISareti (?) koymaniza izin verir! Tamamen Düz Yazi.
  def kargo_yapilabilir_mi?
    @hazir_mi && @fiyat > 0
  end
end


# === NESNELERIN GUCUNU GORELİM ===

# "new" eziyetinden ve parantezlerden cok şık, duzenli atamalar:
benim_siparisim = SiparisKutusu.new("Oyuncu Koltugu", 4500)
benim_siparisim.hazir_mi = true

# IF-ELSE YERİNE (Unless / Eğer Böyle DEĞİLSE):
# Ruby derki 'İf not !(bla == 0)' gibi matematiksel beyin yakan islerle ulasma, 
# 'Unless (Bu değilse)' diye bir kelimem var senin icin!

unless benim_siparisim.kargo_yapilabilir_mi?
  puts "Malesef henuz kutulanmadi, depoda bekliyor."
else
  # "puts" Put String komutu, Console.WriteLine eziyetinin kolayidir:
  puts "Siparisiniz Kuryeye Verildi: #{benim_siparisim.urun_ismi}" 
end


# === HER SEY NESNEDİR! SAYILAR BİLE! (Zarafet Gosterisi) ===

puts "--- Geri Sayim Basladi ---"

# Geleneksel C/Java/PHP tarzi intialized for dongulerine GEREK YOKTUR.
# "10 sayisi(Objesi), Kendinden başlayarak Geriye dogru (Downto), 1 Sayısına(Objesine) ulasana kadar.. donguye(do) gir!"

10.downto(1) do |sayi|
  # Ekrana bas!
  puts "Saniyeler: #{sayi}"
end

puts "Firlat!"
```
Programcının İngilizce gramerini satırlara yedirip (Örn: `10.downto(1)`) muazzam sistemleri 2 kelimeyle saniyeler içinde koşturmasını sağlayan o mucize dil budur.

## Kimler Kullanır?
* Tüm "Start-Up" dünyası: Silikon Vadisindeki bir şirketi ertesi sabah kurup (Kredi kartı veritabanı hazır, HTML viewlar hazır şekilde) dünyanın önüne sunmak isteyen Hızlı-Web Geliştiricileri (RoR Mimarları).
* Stripe veya Shopify altyapısındaki finansal otomatizasyon eklentilerini (Gem'leri) entegre eden Backend mühendisleri.
* DevOps altyapısı kuran, sanal makine ve sunuculardaki dosyaları ve network güvenlik kalkanlarını `Puppet` veya `Chef` (Konfigürasyon betikleri) vasıtasıyla kodlara döken Sistem/Ağ güvenliği (Penesetrasyon uzmanı Hacker) ekipleri.
