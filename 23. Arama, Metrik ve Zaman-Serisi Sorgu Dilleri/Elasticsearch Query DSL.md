# Elasticsearch Query DSL

## Özet
Elasticsearch Query DSL (Domain Specific Language); 2010 yılında Shay Banon tarafından başlatılan devasa Dağıtık Arama Motoru projesi **Elasticsearch** dahilinde, SQL dillerine ait O karmaşık ve sınırlayıcı "JOIN/SELECT" yapılarından kurtulup; Tüm arama, filtreleme, puanlama(Scoring) ve harmanlama (Aggregations) algoritmalarını Doğrudan **Mükemmel İçiçe Giren JSON Objeleri** formatında yazmanızı sağlayan, Endüstrinin 1 Numaralı Büyük Ölçekli Metin Arama(Full-Text Search) Dilidir.

## Nedir ve Ne İşe Yarar?
Eğer 1 Milyoncuk satırlık Kullanıcı Veritabanız varsa SQL `SELECT * FROM tablo WHERE isim LIKE '%Ali%'` Emri iş görür.
AMA! Eğer Netflix, Trendyol veya Wikipedia gibi: İçerisinde Trilyonlarca Cümle, Ürün Açıklaması ve Yorum Bulunan bir Siteniz Varsa; "Ben 'Kırmızı Uçan Kazak' arıyorsam Kırmızı Kazakları Üste Çıkar, Uçan kelimesinde Yakınanlarını da Göster, Bir De Arama Barında Kullanıcının Hata Yaptığı 'KZAK' Kelimesini Tolere Et(Fuzzy Search)" Diyorsanız **SQL ÇÖKER VE YANAR.**

Elasticsearch Dedi Ki: Bütün Bilişimci Arkadaşlarımız! Beni Sadece Bir REST(Okuma) API'si olarak Görün. Bana HTTP İsteği ile Bir **JSON Zarfı (Query DSL)** Fırlatın. Ben İçerideki Trilyonlarca Metin Dosyasını (Ters İndeksleme - Inverted Index Motoruyla) Tarayıp, Saniyenin Milyonda Birinde "İhtimal/Puan (Score)" Sırasına Göre Dizilmiş En alakalı 10 Ürünü Size Fırlatacağım!

**Ne İşe Yarar?**
* **Akıllı Ürün Araması (Full-Text Search):** Her e-ticaret sitesinde arama Çubuğu nun Arka Planı (Elasticsearch/Lucene) 'dur. 
* **Skorlama (Alaka Düzeyi - Relevancy):** Siz Arama Yaptıiğnzda Gelen Ürünlerin Neden İlk Sırada Geldiği Rastgele DEğildir! "Başlığında Geçiyorsa 5 Puan Ver, Açıklamasnda Gecyosrsa 2 Puan Ver Ama Rengi Mavi İse Ekstra 10 Puan KOY!" Diye Programlama Dilini Esnetir...

## Dilin Mantığı ve Kod Yapısı
Sintaksı Tamamen Bir **JSON (JavaScript Object Notation)** Objesidir. 
Tüm Kelimeler (Query, Match, Term, Bool, Filter) Belirli Düğüm (Node) Kuralarına Göre İçe İçe (Nested) Geçeçek Şekilde Kurulur. Programcı Okurken Adeta "Çam Ağacı" gibi genişleyen Bir İkna (Boolean) Meknaizması Yaratır.

### Örnek Bir Elasticsearch Query DSL Kodu: (Netflix/E-Ticaret Usta Arama Motoru Sorgusu!)
Bir kullanıcının "Bilgisayar" Arattığı; AMA "Apple" Markası olanları ve "5000 TL den Büyük Olanları" İSTEYİP "Tablet" olanları Dışladığı O İnanılmaz Puanlama Sorgusu:

```json
/* BU BIR ELASTICSEARCH QUERY DSL (JSON FORMATNDA ARMA) KODUDUR : */
/* Sunuccuya Sadece Bu Zarf Yollahhir!! */
{
  "query": {
      
    // 1. BUYUK (BOOL) KAPSAYICI = Icerise Giren Sartlarin HEpsini Harmanla!
    "bool": {
        
      // A) KESINLIKLE OLMASI GEREKENLER (MUST = SQL'in AND İ!)
      "must": [
        {
          // METIN ARAMASI (Match) : Başlik Kisminda "Oyuncu Bilgisayari" Gecmeli 
          // (Fuzzy: Eger Kulancia 'Bılgisyar' Die Yanlis yazdiya Otomatik DUZELTERK BUL!!)
          "match": {
            "title": {
              "query": "oyuncu bilgisayarı",
              "fuzziness": "AUTO"
            }
          }
        }
      ],
      
      // B) OLMASSA DA OLUR AMA OLURSA DA EN UST SIRAYA TASIMA PUANII(SHOULD)
      // Müşteri Apple sever, Brand Kismi Appleyse O Urunlere +10 Skor Atar! 
      "should": [
        {
          "match": {
            "brand": "Apple"
          }
        }
      ],
      
      // C) DISLAMA (MUST_NOT = Bunlari GetrMe!)
      "must_not": [
        {
          "match": {
            "category": "Tablet" // Tabledler Aramadan Silinsiiin!
          }
        }
      ],
      
      // D) FILTRELEME (FILTER) => Skorpu (Puanlamyi Etkielmeden) Duuz Matematksi Kesim:
      "filter": [
        {
          "range": {
            "price": {
              "gte": 5000,   // Greater Than Equuall(>500) Fiyati Buyku Olanlarki Gtr)
              "lte": 25000   // Lessa Than ( < 25000 Tl)
            }
          }
        }
      ]
      
    } // BOolo Cikis( BİTİs)
  }
}
```
Şu Yukarıdaki JSON bloğunu SQL dilinde Yazsaydınız; Hata toleransı(Fuzzy) gibi özellikler Cihazı Çökertirdi, Üstelik SQL'de "Puanlama(Should)" KAvarmı olmadığı İçin Ürünleri Size "Siparişe Veya isme gÖre" Alakasız Sıralardı. DSL Query, Arananı Bulup "En çok Benzeyenden En Az Benzeyene" Puanlayarak Veren Devrimi Tamamlamkıtır.

## Kimler Kullanır?
* Milyonlarca Kullanıcının Hergnen "Arama (Search)" Yaptığı Tüm Büyük Sistemlerin (Vikipedi , Uber, GitHub vs) **Arka Plan(Gelişmiş Backend) Geliştiricileri ve Arama Mühendisleri**.
* Sunucu Cökelmerini İzleeeyen Siber Güveonmıkçiller; Binlrce Log (Hata Rpaırun) icinden  Sadeece "Fatal ERoor" yzyAnallrai Bu json Diline Filtirlekekrk Günenlik Zaiyefftei Tespiti yaapparlar.. Dünyyanın JSON Tabanli En zengion Dileklerinden Bidiri.
