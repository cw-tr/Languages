# JSON (JavaScript Object Notation)

## Özet
JSON; 2001 yılında Douglas Crockford tarafından (XML'in hantal yapısına isyan ederek) standartlaştırılan, adında "JavaScript" geçmesine rağmen günümüzde **Dilden ve Sistemden Tamamen Bağımsız** olan, internet üzerindeki veri aktarımlarının (API'lerin) %99'unda kullanılan, insanların okuması ve makinelerin saniyede ayrıştırması (Parse etmesi) evrendeki en kolay ve en hafif (Lightweight) **Veri Değişim (Data-Interchange) Formatıdır.**

## Nedir ve Ne İşe Yarar?
1990'larda ve 2000'lerin başında "Hava Durumu Bilgisi" veya "Kullanışlı Hesap Özeti" gibi datalar iki bilgisayar arasında gönderilirken **XML** kullanılıyordu. XML çok ağırdı; `<isim>Ali</isim>` gibi kapatıp açmalardan dolayı veri ağda çok mb yiyordu.

Douglas Crockford, JavaScript dilinin içindeki (Obje yaratma parantezleri olan) `{ }` Kıvırcık Parantez ve `[ ]` Koseli Parantez mantığını Komple Kopyalayarak: "Eğer veriyi Tırnak İçinde İsim ve İki Nokta Üst Üste Değer `{"İsim": "Ali"}` olarak Paketlersek, Bunu internetten yollamak XML'den 100 kat daha az Byte tutar!" dedi. JSON böylece "Web 2.0"'ın (REST API'lerin) tek hakimi oldu.

**Ne İşe Yarar?**
* **Makineler Arası İletişim (RESTful APIs):** Bugün Google'a, Twitter'a veya kendi şirketinizin Veritabanına bağlandığınızda; Oradan Müşterinin telefonuna Akan O Ham Data (Profiliniz, Gönderileriniz) Kesinlikle JSON formatında akar.
* **NoSQL Veritabanı Mimarisi (MongoDB):** MongoDB gibi modern veritabanları, veriyi Excel gibi satır ve sütunlarda değil; DOĞRUDAN JSON (Binary haliyle BSON) formatında diske kazır. Bu sayede Sütun kısıtlamaları kalkar, İsteyen adama "Telefon Numarası" eklenir, İsteyene Eklenmez (Esneklik).

## Dilin Mantığı ve Kod Yapısı
Çook katı ama bir o kadar Basit 4 Kuralı vardır:
1. Veriler Anahtar/Değer (Key/Value) İkilisi halindedir: `"Sehir": "Ankara"`
2. Anahtarlar (Sol Taraf) HER ZAMAN **Çift Tırnak (" ")** içinde olmalıdır. (JavaScript'te tırnak gerekmez ama JSON katıdır).
3. Objeler / Nesneler **Kıvırcık Parantez `{ }`** Araya virgül (,) konularak dizilir.
4. Listeler / Arrayler **Köşeli Parantez `[ ]`** ile dizilir.

Asla içinde Fonksiyon (`function()`) veya Yorum Satırı (`// Yorum`) Barındıramaz! O sadece Saf bir Datalık Bagaj/Valiz dir. Yorum satırı koyarsanız Derleyici Patlar.

### Örnek Bir JSON Kodu: İki Sistem Arasındaki İletişim Paketi (Kargo)
E-Ticaret sitenizin Arkaplanı (Node.js/Python), Müşterinin Tarayıcısına (React/Ekrana) Alışveriş Sepetini yollarken Şöyle Kargo (JSON) Paketler:

```json
{
  "siparis_no": 1045982,
  "kullanici": {
    "isim": "Ahmet",
    "soyad": "Yilmaz",
    "premium_mi": true
  },
  "sepet": [
    {
      "urun_id": 1,
      "baslik": "Mekanik Klavye",
      "fiyat_TL": 1250.50
    },
    {
      "urun_id": 4,
      "baslik": "USB-C Kablo",
      "fiyat_TL": 150.00
    }
  ],
  "notlar": null
}
```
İşte bu kadar kusursuz bir mimari. C# Geliştiricisi bu metni aldığında `DeserializeObject(gelenJson)` kodunu kullanır ve Anında C# nesnesine; Python kullanıcısı `json.loads(gelenJson)` yazar Python Dictionary'sine çevrilir. Yani JSON; Tüm Dillerin İngilizcesi (Ortak Anlaşma Dili)'dir.

## Kimler Kullanır?
* Evrendeki istisnasız BÜTÜN **Yazılımcılar (Front-end, Back-end, Veri Bilimciler, Mobil Geliştiriciler)**. Programlamaya başlayan birisinin ilk öğreneceği Veri Aktarım modeli budur. 
* JSON O kadar popülerdir ki; Eski Dinozor Sistemler (SOAP/XML) bile artık Zorla JSON ile API sunmaya İtilmişlerdir. Hafifliği, Pars (Okunma) Hızı ve Sadeliğiyle Programlama Dünyasının En Değerli Oksijenidir.
