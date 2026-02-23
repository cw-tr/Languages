# XML (eXtensible Markup Language)

## Özet
XML (Genişletilebilir İşaretleme Dili); 1998 yılında W3C tarafından icat edilen, HTML'in aksine verilerin (data) "Ekranda Nasıl Görüneceğine (Tasarımına)" odaklanmak yerine SADECE **"Bu Verinin Anlamı Nedir ve Makineden Makineye (İletişim) Nasıl Güvenli Taşınır?"** felsefesine tapan, Kullanıcının KENDİ ETİKETLERİNİ (Tag) uydurabildiği muazzam derecede katı, resmi ve Evrensel (Eski Sistemlerin Kralı) bir Veri-Tutma/İşaretleme (Data Serialization) dilidir.

## Nedir ve Ne İşe Yarar?
1990'larda HTML harikaydı, web sitelerini çiziyordu. Lakin bir Hava Yolu şirketi, kendi Uçak Bilet verilerini (Adı: Ali, Fiyatı: 50TL, Uçuş: THY) bir Bankanın bilgisayarına (Web sitesine değil, koduna/API'sine) fırlatmak istediğinde Ortak Bir Dil Formatına ihtiyaç vardı. 

Bankanın Bilgisayarı "CSV (Virgüllü listelerden)" anlamayabilirdi. "HTML" yollasalar HTML'in içinde biletin fiyatı `<p>50</p>` diye yazardı, Bilgisayar Paragraf Parantezini görünce neresi fiyat neresi isim Karıştırıp Algılayamazdı! 

W3C, "HTML gibi Küçüktür/Büyüktür `< >` Oklarına Sahip olan, AMA İsteyen Firmanın **KENDİ İSTEDİĞİ KELİMELERİ/Tagları** İcat Hürriyetine Sahip olduğu bir Veri Formatı" Cıkardı: XML! Artık Banka `<Fiyat ParaBirimi="TL">50</Fiyat>` diye harika makinece okunan veriler çekebiliyordu.

**Ne İşe Yarar?**
* **Makineler Arası İletişim (SOAP / Web API/ WEB Services):** Bankacılık, e-Devlet, Sağlık Sistemlerinin o Eski / Dev ve katı sunucu aktarımlarında (Sen bana TCKN at, ben sana XML içinde Adamın Mühürlü Suç sicilin döneyim) C# ve Java programlarının iletişiminde Mutlak Krallıktı. (Günümüzde krallığını REST/JSON'a kaptırmıştı).
* **Konfigürasyon Dosyaları:** Android Programlama'da Ekranın tasarımı (Button şurada dursun, arka planı şu olsun) Java ile YAZILMAZ; `<Button length="match_parent"/>` diye XML Ayar şablonlarıyla Çizilir. Android (Java/Kotlin) UI katmanında, Spring Boot'da (Pom.xml/Maven) konfigürasyon yapılarında beyni temsil eder.

## Dilin Mantığı ve Kod Yapısı
Mantığı HTML'e saniyesinde benzer, ama KATI kuralları vardır:
1. XML Asla kendi kendine bir şey çizmez, O sadece Çıplak bir DATA(Metin) hamalıdır.
2. HTML'de kapanmayan etiketler `<img src="">` hata vermez çalışır. XML'de, eğer Açtığın Etiketi `<kitap>` anında Kapatmazsan `</kitap>`, Kod ANINDA Patlar / Ölür (Well-Formed Zorunluluğu). Küçücük bir sintaks eksiği bütün Trilyonluk Banka Transferini Durdurur (XSD Validasyonu).

**Örnek İşleyiş (Sembolik Olarak):**
CSV'de Data: `Ali, 25, Yazilimci` (Okuyan kisi bunun Yas mı ayakkabi nuasi mi oldugunu sutun sirasindan bilir/Karışabilir).
XML'de Data: `<Calisanlar><Kisi><Ad>Ali</Ad><Yas>25</Yas></Kisi></Calisanlar>` (1000 yıl geçse de neyin Verisi/Metni olduğunu Unutamazsınız, Açıklaması Boynundadır/Semantiktir).

### Örnek Bir XML Kodu: Kütüphane Kitap Aktarım API'si (Sözleşmesi)
Eğer Amazon.com'dan Kendi sitenize bir Kitap Datası Entegre/API ile çekiyorsanız Sistem C# uygulamanıza bu String (XML) hamal ağacını fırlatır:

```xml
<?xml version="1.0" encoding="UTF-8"?>
<!-- 1. ZORUNLU KAPSAYICI(ROOT): XML Her zaman En disinda TEK BIR Ahtapotun kollarini iceren "Kök" bir Mühr() barindirmak zorundadir. Burada o : "Kutuphane" dir -->

<Kutuphane xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">
    
    <!-- 2. KENDİMİZ ÜRETTİGİMİZ DATA TAGLARI (ÇIRAKLAR): Sirayla Nesneleri(Objeleri) Sirala -->
    <!-- "Kategori" Bir (Attribute / Özelliktir (Yani ana veri değil, onu niteliklendiren meta-veri tagi) -->
    
    <Kitap kategori="BilimKurgu" BaskiYili="1965">
        <Baslik>Dune</Baslik>
        <Yazar>Frank Herbert</Yazar>
        <SayfaSayisi>896</SayfaSayisi>
        <Fiyat Birim="USD">14.99</Fiyat>
    </Kitap>

    <Kitap kategori="Yazilim" BaskiYili="2020">
        <Baslik>Clean Architecture</Baslik>
        <Yazar>Robert C. Martin</Yazar>
        <!-- DiKKAT: Fiyatı bilinmiyor olabilir, Kendini ANINDA KAPATAN (Self-Closing) minik (/) ETIKET!: -->
        <StoktaYok/> 
        <!-- <StoktaYok></StoktaYok> Da yazabilirdik lakin zaman (Byte) tasarrufu -->
    </Kitap>

</Kutuphane>
```
Bir C# Geliştiricisi bu XML dosyasını programla indirdiğinde; sistemin (Parser) yapay zekasına şunu der: `"Bana XML Agacının (XPath) icindeki > Kutuphane > Kitap > Yazar Isimlerini Foreach dongusuyle Liste yap!"`. Böylece 1 milisaniyede içinden "Frank Herbert" i kelime Cımbıklaması gibi alır çeker.

## Kimler Kullanır?
* Evrendeki bütün **Eski-Nesil Katı Kurumsal Geliştiriciler (Banks, Devlet SOAP API'leri) ve Java / C# / .NET Arka plan Konfigürasyoncuları (Backend)**.
* **GÜNÜMÜZDE BÜYÜK BİR DÜŞÜŞTÜR:** Neden mi? Çünkü XML, yukarıda da gördüğünüz gibi "Ali" kelimesini aktarmak için Ekrana (Veriye/Diske) Fazladan `<Ad> Oraya Buraya ... </Ad>` kelimelerini basıp Verinin Megabayt (Ağ Kotasını) devasa şişirmektedir!(Verbosity / Amele Kalabalığı).
* Bu sebepten dolayı, 2010 yıllarından Sonra Dünyadaki Tüm İletişim Datası (Veri Transferi); Etiketi söküp/atıp Kıvırcık Parantez `{ "Ad" : "Ali" }` ile JavaScript Mimarisine sığınmış **JSON (JavaScript Object Notation)** Formatına Savaşını Kaybetmiştir. Bankalar dışında (REST/JSON) kraldır. Sadece App ayarlamalarında (Android/VS) varlık sürer.
