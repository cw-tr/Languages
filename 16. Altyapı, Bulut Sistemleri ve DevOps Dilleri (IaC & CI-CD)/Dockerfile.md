# Dockerfile

## Özet
Dockerfile; 2013 yılında Solomon Hykes (Docker A.Ş) tarafından icat edilen Konteynerizasyon (Containerization) devriminin kalbidir. Programlama dili olmamasına rağmen; Yazılımcıların kendi bilgisayarlarında çalışan kodun **"Benim makinemde çalışıyordu, Müşterinin veya Sunucunun makinesine(Linux) atınca çöktü (!)"** Kabusunu bitiren, bir yazılımın "Nasıl Paketleneceğini, Hangi İşletim Sistemine İhtiyacı Olduğunu ve Bağımlılıklarını" tek bir dosyada Zırh gibi kaplayan (Build) Standart Talimatname (Script) formatıdır.

## Nedir ve Ne İşe Yarar?
2013'ten önce Üniversitede (Veya Şirkette) Bir Java (veya PHP) projesi yapardınız. Arkadaşınıza (veya Yayın Sunucusuna) exe/kod olarak atardınız. Arkadaşınız Açar, *"Hata! Bilgisayarda Java 8 yok!"*, *"Hata! Makinenin MySQL veritabanı Sürümü Eski Çöktü"* diye Ağlardı. Programları çalıştırmak için Hedef makineye Binlerce ÇEVRE (Environment) programı kurmak Amelelikti.

Docker Dediki: Kardeşim, Sadece Uygulamanın Kodunu DEĞİL, Uygulamanın Üstünde Koştuğu O Şirin **(Kendi İşletim Sistemini, Java'nın kendi versiyonunu, kendi Klasörünü) Kutu İçinde Kutu (Konteyner)** Olarak Kargo Etsenize! Böylece Adman Makinesinde (Docker Yüklüyse) Sizin uygulamanız *Dünyadan İzole Olmuş Küçücük bir Sanal Kutuda* Eksiksiz Şekilde Oynar! İşte bu Kutuyu üreten Kalıbın Adı **Dockerfile**'dır.

**Ne İşe Yarar?**
* **Her Yerde Aynı Çalışma (Write Once, Run Anywhere):** Uygulamanızı Bir kere Desktop'da (Windows) Docker'a koyarsanız, O Konteyner Uzaya da gitse, AWS Bulutuna da gitse, Arkadaşının eski laptobuna da gitse **Aynı Saniyede GRAM Hata vermeden Başlar!** Çünkü Bağımlılıkları Kutunun İçindedir!
* **Mikroservisler (Microservices):** Şirketlerin Bir Ana uygulaması çökünce hepsi çökerdi. Docker'la "Kullanıcı Giriş Sunucusu" Ayrı Kutuda, "Sepet Sunucusu" Ayrı kutuda (Birbirinden izole) yaşar.

## Dilin Mantığı ve Kod Yapısı
Tamamen Kendi Özel, Büyük Harflerle (MAJUSKÜL) ve Emirlerle Yazılan Yönergelerden (Instructions) oluşur. İşletim Sistemi Bash(Terminal) Shell mantığıyla Birebir Çalışır.

**Temel Emirler (Talimatlar):**
1. **`FROM`**: Hangi İşletim Sistemi üzerine İnşa edelim? (Örn: Alpine Linux, Ubuntu 20.04)
2. **`WORKDIR`**: Bilgisayarın İçindeki Sanal Kutuda Hangi Klasörde Duracağız?
3. **`COPY`**: Senin Kendi C Diskinde Tutan Kodu, -> Kutunun İçine Kopyala / Enjekte Et!
4. **`RUN`**: Kutunun Içinde İnterneten Uygulamanin Bagimliliklarini Indir (Kurulum Yap).
5. **`CMD`**: Bu konteyner (Kutu) Müşteride Açıldığı anda (RUNlandığında) Hangi Düğmeye Basıp Projeyi Ayağa Kaldırayım?

### Örnek Bir Dockerfile Kodu: Bir Node.js Web Uygulamasının Zırha (Konteynere) Alınması!
Diyelim ki bir Web Siteniz(API) var. Bunu Canlı Sunucuya(AWS) taşımak için Paketleme(Dockerize) Emiri:

```dockerfile
# 1. TEMEL ALTYAPI (ISLETIM SISTEMI SECIMI)
# Kutumuzun (Isletim sisteminin) Kalbi, Iceisinde Hazir Nodejs(18. Surumu) Kurulmus Alpine Linux Olsun! 
# (Alpine inanilmaz hafiftir, Sadce 5 Megabyte Isletim Sistmeridir).
FROM node:18-alpine

# 2. CALISMA ALANI 
# Kutunun icindeki Sanal Bilgisayarda, Su klasore gec! (Eger yoksa kendi yaratir).
WORKDIR /app

# 3. ICERI VERI KOPYALMA 
# Senin (Gercek Masaustundeki) Paket ayar dosyani(package.json) -> Kutunun Icine(.) Kopyala!
COPY package*.json ./

# 4. UZAY KUTUSUYLA(LINUXLA) ETIKLESIIM, BINDEMELERI(NPM KUTUPHANESINI) KUR.
# Bu kod imaj derlenirken ccalisir. İhtiyaclar Icene gomülür.
RUN npm install

# 5. GERI KALAN HER SEYI (Senin JS Kodlarini/HTML'lerini vs) KUTUYA YIG!
COPY . .

# 6. PORT (DISA ACILMA)
# Bu izole kutunun 3000 kapısı(Portu) acikdir. Internet Trafiklerini iceri burdan al diyoruz.
EXPOSE 3000

# 7. BASLAMA DUGMESI (KOMUT)
# Müşteri (Veya Bulut Sunucusu) bu Kutuyu Satın Alip Çalistitgirdignda Cekilecek En Son TETIK (CMD):
CMD ["node", "app.js"]

```

Yazılımcı Bu Dosyayı Yazıp Terminalde `docker build -t benimuygulamam .` Dediğinde, Docker Gider Linuxu (Alpine) indirir, Kodu İçine tıkar Dondurur ve "Bir İMAJ(Kalıp/İso)" ortaya Çıkarır. Bu kalıp Milyarca kez çoğaltlıp çalıştırabilir! Ortam Çökmeleri Bitmiştir!

## Kimler Kullanır?
* Gezegendeki İstisnasız Tüm **DevOps Mimarları, Full-Stack Yazılımcıları, Sunucu Yöneticileri**.
* Dockerfile'lar, **Kubernetes (K8s)** Dedigimiz Devasa Orkestranın Askerleridir. Kubernetes (Limandaki Kaptan), Binlerce Dockerfile Cıktısı olan (Konteyneri/Kutuyu) Gemiye Aynı ANDA Uyuma sokup cemaatyapan Google Çıkışlı Sistemin Adıdır. Her Yazılım Mühendisi Ölmeden Önce Konteyner Mimarisine Mecbur Veda Edecektir.
