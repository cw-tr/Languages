# GraphQL

## Özet
GraphQL; 2012 yılında Facebook (Meta) tarafından geliştirilen ve 2015'te açık kaynak yapılan, geleneksel REST API mimarisinin "aşırı veri (Over-fetching)" veya "eksik veri (Under-fetching)" sorunlarını kökünden çözen, istemcinin (Client) **"Sadece ve tam olarak neye ihtiyacı varsa Onu istediği (Sorguladığı)"** devrimsel bir Veri Sorgulama (Query) Kütüphanesi ve API Tasarım Mimarisidir.

## Nedir ve Ne İşe Yarar?
Eskiden (ve hala REST kullanan sistemlerde) bir kullanıcının Profil sayfasını çizmek için, veritabanına `GET /users/1` isteği atardınız. Sistem size kullanıcının **Bütün Bilgilerini (Adı, şifre hash'i, Adresi, 10 yıllık post geçmişi)** 5 Megabyte JSON olarak geri fırlatırdı. Fakat sizin ekranda SADECE "Adı ve Profil Fotoğrafı" basmanız gerekiyordu! O Fazla gelen veri (Over-fetching) kullanıcının internet Üniversitesini/Kotasını ve Telefonun Şarjını yiyordu.

Facebook (Trilyonlarca Datası olduğu için) bu mobil veri isyanını durdurmak adına GraphQL'i icat etti. GraphQL dedi ki: "Bana Tek Bir Uçtan (Endpoint) bağlan! Bana Göndereceğin JSON sorgusunda Hangi Tarlaları (Field) İstediğini Belirt. Geri kalan Hiçbir çöpü sana Atmayacağım!".

**Ne İşe Yarar?**
* **Mobil Uygulama Optimizasyonu:** Mobil cihazlar (Ağ gecikmesi yüksek olan 3G/4G şebekeleri) çok fazla istek atıp veri indirmesin diye, Tek 1 İstekte Müşterinin tam istediği Gramajda veriyi döndürüp İnternet performansını uzaya fırlatır.
* **Agility (Hızlı ÖnYüz Geliştirme):** Front-end gelişticisi "Abi şu endpoint'e kullanıcının Soyadını da Eklersenize!" diye Arka-Plan (Backend) mühendisine yalvarmak zorunda kalmaz. Front-End'ci doğrudan Çektiği Sorguya `soyad` yazar ve anında gelir, Backend kodunu güncellemeye Gerek kalmaz!

## Dilin Mantığı ve Kod Yapısı
GraphQL'in tam anlamıyla bir Veritabanı (Database) OLMADIĞINI akılda tutmak gerekir (O bir Sorgu dilidir, Arka planda yine SQL veya MongoDB çalışabilir).
İki temel Yüzü vardır:
1. **Schema (Veri Ağacı):** Backend'in (Sunucunun) elinde Ne veriler var Onların Tip(Type) beyanı (Örn: `type User { id: ID, name: String }`)
2. **Query/Mutation (İstekler):** Client'in (Senin) Sunucuya yolladığın İstek şablonudur. Veri çekerken `query`, Veri değiştirirken (Update/Insert) ise `mutation` kullanılır.

**Örnek İşleyiş:**
Klasik REST: `https://api.site.com/users/5/friends` (10 İstek atmanız gerekebilir).
GraphQL: Tek `https://api.site.com/graphql` adeline şu Sorguyu atarsınız: Mükemmel JSON döner.

### Örnek Bir GraphQL Kodu: İstek Atma Mimarisi (Müşteri Gözünden)
Örneğin siz Frontend Geliştiricisiniz ve Bir E-Ticaret / Sosyal Medya sayfasında "Kullanıcı Ali'nin Adını, Ve Ali'nin Arkadaşlarından "SADECE İsimlerini" Çekmek istiyorsunuz (Arkadaşların şifreleri, yaşları vs lazim degil zira Ekrana Sigmaz!):

```graphql
# BU BIR GRAPHQL SORGUSUDUR (REST URL'leri yerine kullanilan Gövde Dili)

query KullaniciProfiliveArkadaslariniCek {
  # 15 Numarali ID'si olan User'i Hedefle:
  user(id: "15") {
    
    # BANA SADECE ID, ISIM VE EMAIL DÖN! (Telefon numarasini veya Parolayi Gonderme, Tasarruf Et!)
    id
    isim
    email
    
    # 2. SEVIYE ILISKI GRAPH'I Taraması(JOIN yapmadan İçi içe Nesne Sihri):
    # Bu adamin Arkadas Listesini (Array) bana Don!
    friends(durdur: 5) { # Sadece Ilk 5 Arkadasini cek
        
      # Gelen 5 Aarkdasin da SADECE isim ve Resmini Getir(Arkadasin Baska Verisini indirme)!
      isim
      profilResmi
      
    }
  }
}
```

Eğer Yukarıdaki İsteği (Tek 1 Satır JSON/HTTP ile) Sunucuya şutlarsanız, Sunucunun Size Döneceği 1 Milisaniyelik O Pürüzsüz Veri Formatı Şuduuur:

```json
{
  "data": {
    "user": {
      "id": "15",
      "isim": "Alperen",
      "email": "alp@web.com",
      "friends": [
        { "isim": "Meryem", "profilResmi": "url1.jpg" },
        { "isim": "John", "profilResmi": "url2.jpg" }
      ]
    }
  }
}
```
Hiçbir Gereksiz Byte Yüklenmedi. Hiçbir Exra Array İndirilmedi. Kusursuz bir "Ne İstediysen Onu Altın Tepside Aldın" Mimarisi. (Bu Yüzden Adı Graph Query Languagedir).

## Kimler Kullanır?
* Evrendeki bütün Şirketlerin **Modern API Tasarım Şefleri, Full-Stack Developerlar**. (React Geliştiricileri Apollo GraphQL kancasını kullanarak REST'in axios'undankurtulur).
* Eskiden Github API'si sadece REST ti. Sırf performans çöktüğü İşin GitHub bile 2017'lerde (V4 ile) Tamamen GrahQL'e geçti ve Dedi ki "API'mi sömürenler, Sadece ne istiyorsa o sütun adlarını belirtsin, Sunucumu Yormasın". 
* **Shopify, X(Twitter), Facebook, Pinterest;** milyonlarca İlişkisel Çizge(Graph) datasının Ekrandan Sorgulanması İçin Gözü kapalı bunu kullanır. REST API mimarisinin En güçlü Celladıdır.
