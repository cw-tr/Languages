# Protocol Buffers (Protobuf)

## Özet
Protocol Buffers (kısaca Protobuf); 2008 yılında **Google** tarafından icat edilen, makinelerin birbiriyle (Server-to-Server, Microservices) veri transferi yaparken JSON ve XML'in o yavaş metin (String) tabanlı aktarımını Paramparça(Optimize) ederek çöpe atan; Milyarlarca veriyi **Binary (İkilik 0 ve 1)** sistemine Sıkıştırıp Sergileyen(Serialize) dünyanın tartışmasız en Güvenilir, En Ölçeklenebilir ve **En Performanslı RPC (Remote Procedure Call)** Veri Değişim Formatıdır.

## Nedir ve Ne İşe Yarar?
Google; Dünya üzerindeki Saniyede Milyarlarca (Gmail, YouTube, Maps) aramasını kendi sunucuları arasında Çekerken Başta XML veya JSON kullanıyordu. 
Diyelim ki JSON ile Ali'nin yaşını(25) Yolluyoruz: `{"isim": "Ali", "yas": 25}`...  
Google Mühendisleri dedi ki: **"Biz Neden aptal gibi her seferinde (isim) ve (yas) KELİMELERİNİ Milyar kere İngilizce Harfle Agdan karsiya gonderiyoruz? Bu devasa bir Bant-Genisligi (Network Bandwith) israfıdır!"**

Protobuf İcat edildi: Protobuf dedi ki! İki Sunucu arasına "Sözleşme (Schema .proto DOSYASI)" Koyalım. Birinci kutu `isim`, İkinci kutu `yas` olacak diyelim. Ve Biz Karşıya (JSON Metni) yollamak Yerine Sımsıkı Sıkıştırılmış İkilik(Binary Zip) formatı yollayalım! "Ali25". Karşıdaki makine Şemaya bakarak O Gelen "Gizli sıkıştırılmış ZİP'i" milisaniyede Açar. Sonuç? JSON'dan 10 KAT DAHA KÜÇÜK VE 100 KAT DAHA HIZLI Sunucu İletişimleri!

**Ne İşe Yarar?**
* **Microservis Mimarisi (gRPC):** Günümüzde YemekSepeti, Trendyol, Uber, Netflix gibi Firmaların arkaplanında "Sepet Sunucusu" İle "Ödeme Sunucusu" Kendi Aralarında Konuşurken ASLA Yavaş olan REST-JSON API Kullanmazlar. gRPC (Google Remote Procedure Call) yani Arkaplandaki o Fırtına Protobuf Protokolünü Kullanarak Milisaniyede şifreli(Binary) iletişim yaparlar.
* Tip Güvenliği (Strict Typing): Protobuf'ta yaş rakamına (int32) Harf atarsanız C++ Değerind Derleyici direk Çöker, JSON daki gibi Üretime(Production) O hata Sızamaz!

## Dilin Mantığı ve Kod Yapısı
Siz projede `.proto` Uzantılı Tek bir Kural Kitabı yazarsınız. O kitabın Üzerinde "Protobuf Compiler (protoc)" komutunu Calistirirsiniz!
O Compiler; Kural kitabını okur ve Sizin İçin: Java Kodlarını, C# Kodlarını ve Go Kodlarını (Classları/Yapıtaşlarını) OTOMATİK OLARAK Oluşturup Sizin Proje Dosyalarınıza Hediye Eder! Siz bir daha Model Tanımlamazsınız!

### Örnek Bir Protobuf Kural Şeması (.proto) ve Arka Plandaki Sihri
Aşağıda Google'ın o Ünlü Protobuf Sözleşmesinde, İki Sunucu "Kişi (Person) Datasını nasıl Sıkıştıracak" Diye verdikleri O Sözleşme Kodu:

```protobuf
// Kural 1: Modern Protobuf Versiyon 3 u Kullaniyoruz!
syntax = "proto3";

// Bu Sözlesmeden Otomatik java veya C# Çikartmak İstiyorsan Paket/Namespace tanımı:
package KullaniciIslemleriAPI;

// 1. SÖZLEŞME NESNESI (Message: Java'daki CLASS veya JSONdaki Obje):
message Kisi {
  
  // Degisken Tipi | Degisken Ismi | ESITTIR RAKAM(Sira Numarasi) 
  
  string isim = 1;        // Veri sıkıştırılırken bu 1. Siradaki(Slot) Veridir Ekrana İsim vs diye kelime atilmaz! 1 diye Gomer Gecer.
  int32 id_numarasi = 2;  // Rakam (32 bit Int). 2 Sirayi Alir
  string e_posta = 3;     // Kisisinin Maili, 3. Sirayi Alir.
  
  // Dizi (Array/List) KAvrami Protobufta "Sürekli Tekrar Edebilir (repeated)" demektir:
  repeated TelefonNumarasi numaralar = 4;
}

// 2. Alt/Içice Baska Bir SOZLESME (Enumeration ve Nested Clas)
message TelefonNumarasi {
  string numara_tel = 1;
  
  // ENUM(Degisken Secenekleri) : Ev veya Cep telefonu Olabilir!
  enum TelefonTipi {
    MOBIL = 0;   // Tip  0'dir (En az 1 byte yer Tutar JSON'daki "Mobil" Harflerini Agda Tasimazz!)
    EV = 1;
    IS_YERI = 2;
  }
  
  TelefonTipi tip = 2;
}
```

Eğer bir Adamın Verisini Çekerseniz. Protobuf Bunu Ağa(İnternet kablosuna) Şu Halde basar: `0x12 0x07 0x41 0x6c 0x69 0x22 ...`.  Bu yüzden Bir İnsanın Bunu Okuması İMKANSIZDIR. Fakat Ekran Kartının/İşlemcinin (CPU) bu Binary Formattaki Yapıyı Parçalaması (UnZip) JSON Stringine Göre Yüzlerce Defa hızlıdır. Ve bu Google'ın Ayakta Kalma Sırrıdır.

## Kimler Kullanır?
* Evrendeki bütün (Kendi Sunucularının Arka Planda Kendi Kendisiyle Konuştuğu Ağlarda / REST Dışı API'lerde) **Usta Backend Geliştiricileri (Java, Go, C# Mimarları)**.
* Oyun Sunucuları: Online Oyunlarda (FPS vs) "Mermiyi sıktı X=Y=Z koordinatları" gibi saniyede Milyonlarca Datayı Karşıya (Sunucuya) Şutlarken Oyuncu Gecikme (Ping) yaşamasın diye Bütün Multiplayer Ağ İstekleri JSON yerine Binary (Örn. Protobuf) mimarisiyle kodlanır. Endüstrinin Uç Uça Demir köprüsüdür.
