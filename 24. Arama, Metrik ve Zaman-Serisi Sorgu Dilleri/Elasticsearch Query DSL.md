# Elasticsearch Query DSL

## Özet
Elasticsearch Query DSL (Domain Specific Language); 2010 yılında Shay Banon tarafından başlatılan devasa Dağıtık Arama Motoru projesi **Elasticsearch** dahilinde, SQL dillerine ait O karmaşık ve sınırlayıcı "JOIN/SELECT" yapılarından kurtulup; Tüm arama, filtreleme, puanlama(Scoring) ve harmanlama (Aggregations) algoritmalarını Doğrudan **Mükemmel İçiçe Giren JSON Objeleri** formatında yazmanızı sağlayan, Endüstrinin 1 Numaralı Büyük Ölçekli Metin Arama(Full-Text Search) Dilidir.

## Nedir ve Ne İşe Yarar?
Eğer 1 Milyoncuk satırlık Kullanıcı Veritabanız varsa SQL `SELECT * FROM tablo WHERE isim LIKE '%Ali%'` Emri iş görür.
AMA! Eğer Netflix, Trendyol veya Wikipedia gibi: İçerisinde Trilyonlarca Cümle, Ürün Açıklaması ve Yorum Bulunan bir Siteniz Varsa; "Ben 'Kırmızı Uçan Kazak' arıyorsam Kırmızı Kazakları Üste Çıkar, Uçan kelimesinde Yakınanlarını da Göster, Bir De Arama Barında Kullanıcının Hata Yaptığı 'KZAK' Kelimesini Tolere Et(Fuzzy Search)" Diyorsanız **SQL ÇÖKER VE YANAR.**

Elasticsearch Dedi Ki: Bütün Bilişimci Arkadaşlarımız! Beni Sadece Bir REST(Okuma) API'si olarak Görün. Bana HTTP İsteği ile Bir **JSON Zarfı (Query DSL)** Fırlatın. Ben İçerideki Trilyonlarca Metin Dosyasını (Ters İndeksleme - Inverted Index Motoruyla) Tarayıp, Saniyenin Milyonda Birinde "İhtimal/Puan (Score)" Sırasına Göre Dizilmiş En alakalı 10 Ürünü Size Fırlatacağım!

**Ne İşe Yarar?**
* **Akıllı Ürün Araması (Full-Text Search):** Her e-ticaret sitesinde arama çubuğunun Arka Planı (Elasticsearch/Lucene)'dur. 
* **Skorlama (Alaka Düzeyi - Relevancy):** Siz Arama Yaptığınızda Gelen Ürünlerin Neden İlk Sırada Geldiği Rastgele Değildir! "Başlığında Geçiyorsa 5 Puan Ver, Açıklamasında Geçiyorsa 2 Puan Ver Ama Rengi Mavi İse Ekstra 10 Puan KOY!" Diye Programlama Dilini Esnetir...

## Dilin Mantığı ve Kod Yapısı
Sintaksı Tamamen Bir **JSON (JavaScript Object Notation)** Objesidir. 
Tüm Kelimeler (Query, Match, Term, Bool, Filter) Belirli Düğüm (Node) Kurallarına Göre İçe İçe (Nested) Geçecek Şekilde Kurulur. Programcı Okurken Adeta "Çam Ağacı" gibi genişleyen Bir İkna (Boolean) Mekanizması Yaratır.

### Örnek Bir Elasticsearch Query DSL Kodu: (Netflix/E-Ticaret Usta Arama Motoru Sorgusu!)
Bir kullanıcının "Bilgisayar" Arattığı; AMA "Apple" Markası olanları ve "5000 TL den Büyük Olanları" İSTEYİP "Tablet" olanları Dışladığı O İnanılmaz Puanlama Sorgusu:

```json
/* BU BIR ELASTICSEARCH QUERY DSL (JSON FORMATINDA ARAMA) KODUDUR : */
/* Sunucuya Sadece Bu Zarf Yollanır!! */
{
  "query": {
      
    // 1. BÜYÜK (BOOL) KAPSAYICI = İçerisine Giren Şartların Hepsini Harmanla!
    "bool": {
        
      // A) KESİNLİKLE OLMASI GEREKENLER (MUST = SQL'in AND'i!)
      "must": [
        {
          // METİN ARAMASI (Match) : Başlık Kısmında "Oyuncu Bilgisayarı" Geçmeli 
          // (Fuzzy: Eğer Kullanıcı 'Bılgisyar' Diye Yanlış yazdıysa Otomatik DÜZELTEREK BUL!!)
          "match": {
            "title": {
              "query": "oyuncu bilgisayarı",
              "fuzziness": "AUTO"
            }
          }
        }
      ],
      
      // B) OLMASA DA OLUR AMA OLURSA DA EN ÜST SIRAYA TAŞIMA PUANI (SHOULD)
      // Müşteri Apple sever, Brand Kismi Appleyse O Urunlere +10 Skor Atar! 
      "should": [
        {
          "match": {
            "brand": "Apple"
          }
        }
      ],
      
      // C) DIŞLAMA (MUST_NOT = Bunları Getirme!)
      "must_not": [
        {
          "match": {
            "category": "Tablet" // Tabletler Aramadan Silinsin!
          }
        }
      ],
      
      // D) FİLTRELEME (FILTER) => Skoru (Puanlamayı Etkilemeden) Düz Matematiksel Kesim:
      "filter": [
        {
          "range": {
            "price": {
              "gte": 5000,   // Greater Than Equal (>=500) Fiyatı Büyük Olanları Getir)
              "lte": 25000   // Less Than ( < 25000 TL)
            }
          }
        }
      ]
      
    } // Bool Çıkış (Bitiş)
  }
}
```
Şu Yukarıdaki JSON bloğunu SQL dilinde Yazsaydınız; Hata toleransı (Fuzzy) gibi özellikler Cihazı Çökertirdi, Üstelik SQL'de "Puanlama (Should)" Kavramı olmadığı İçin Ürünleri Size "Siparişe Veya İsme Göre" Alakasız Sıralardı. DSL Query, Arananı Bulup "En çok Benzeyenden En Az Benzeyene" Puanlayarak Veren Devrimi Tamamlamıştır.

## Kimler Kullanır?
* Milyonlarca Kullanıcının Her gün "Arama (Search)" Yaptığı Tüm Büyük Sistemlerin (Vikipedi, Uber, GitHub vs) **Arka Plan (Gelişmiş Backend) Geliştiricileri ve Arama Mühendisleri**.
* Sunucu Çökmelerini İzleyen Siber Güvenlikçiler; Binlerce Log (Hata Raporu) içinden Sadece "Fatal Error" yazanları Bu JSON Diline Filtreleyerek Güvenlik Zafiyeti Tespiti yaparlar. Dünyanın JSON Tabanlı En zengin Dillerinden Biridir.
