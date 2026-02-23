# YAML (YAML Ain't Markup Language)

## Özet
YAML; 2001 yılında Clark Evans tarafından icat edilen, JSON'un o kalabalık (Sürekli parantez ve tırnak kapama) eziyetini çöpe atıp SADECE **"Boşluklar ve Girintiler (Indentation / Python tarzı tablamalar)"** kullanarak yazılan, dünyanın Açık ara "En İnsancıl Okunabilir (Human-Readable)" Ayar/Data ve Konfigürasyon Veri Formatıdır.

(İsmi önceden Yet Another Markup Language'ti; Sonradan HTML(Biçimlendirme) olmadıgını Veri odugunu vurgulamak icin Öz yinelemeli bir Şakayla "YAML Ain't Markup Language - Yaml bir işaretleme dili degildiR" yapılmıştır).

## Nedir ve Ne İşe Yarar?
Eğer bir Programın/Sunucunun Ayarlarını JSON ile yazarsanız (`{ "port": 8080 }`), araya yorum (Comment) satırı ekleyemezsiniz ve sürekli Süslü Parantezleri `{ }` unutup Hata(Crash) çıkarırsınız. Bu Bilgisayarla (DevOps) Sunucu mimarisi Kuran Sistem Yöneticileri içi Kabustur.

YAML Dedi ki: Bütün Tırnakları, Virügülleri, Parantezleri silin! Sadece Boşluk Bırakırsanız Sistemi hiyerarşik (İnner/İçeri) Soktuğunuzu anlarım. Ortaya O kadar güzel ve Temiz bir "Ayar (Config) Dosyası" Çıktı ki, DevOps(Sunucu Mimarisi) mühendisleri ona aşık oldu.

**Ne İşe Yarar?**
* **DevOps Ve CI/CD (Sürekli Entegrasyon) Ayarları:** Docker Compose (`docker-compose.yml`), Kubernetes Manifestoları, GitHub Actions (`.github/workflows/main.yml`) ve GitLab CI sistemlerinin Tamamı YAML üzerine kuruludur. Bir Sunucuya "Bana 2Tane Ubuntu Linux Kur" demek 2 satır Yaml kodudur!
* Her şeyden öte; İçine **Yorum Satırı (`#`)** Konabildiği için (JSON'da Bu yoktur) Konfigürasyonların En Vazgeçilmez Yaratıcısıdır.

## Dilin Mantığı ve Kod Yapısı
Hiçbir Parantez Yoktur. Tamamen Girinti (Indentation) sistemidir (Akrepler/Boşluklar önemlidir - Tab tusu kullanılmaz 2 veya 4 boşluk bırakılır).

1. **Key: Value (Değer):** `name: Ali` (Tırnaksız direkt Değerler!).
2. **Listeler (Dizi/Array):** Başına Sadece Tire `-` Çekilir.
3. İç ice giren Objelerde Sadece Satır Altına geçip 2 Kere (Space) Boşluğa basmak Yeterlidir.

### Örnek Bir YAML Kodu: Kubernetes veya GitHub Action (Sunucu Komutu) Ayar Dosyası Mimarisi!
Bir Programcı (veya DevOps) Uzaktaki bir Bulut (AWS) Sunucusuna "Hangi makineleri ne sırayla kaldıracagını" Su siirsel Yaml formatıyla Emir verir:

```yaml
# BU BIR YAML (AYAR/CONFIG) DOSYASIDIR. (Yorum satirlari Python Gibidir)

# Genel Servislerimiz
version: '3.8'

services:
  # 1. MAKINE: BENİM YAZDIĞIM WEB SITESI! (Nodejs)
  web_api:
    image: node:14            # Internetten Hazir Node.js Sistem Klonu İndir
    container_name: cyber_app # Benim Sanal Sunucumun Adi
    ports:
      - "8080:3000"           # Benim 8080 Portumu Icerideki 3000 iLe Eşle (Liste / Tire)
    environment:              
      - NODE_ENV=production   # Yayina alindigi Mod (Aslinda bu bir Dizidir(Array) tireyle Belirtiriz)
      - DATABASE_URL=mysql://kullanici:sifre@db:3306/benimdb

  # 2. MAKINE: VERITABANIM (Yandaki Makine)
  veritabani_sistemi:
    image: mysql:8.0          # MySQL'in En gunce ve hazir Verisonunu Yukle
    container_name: cyber_db_mysql
    restart: always           # Eger Sunucu cokersen Otomatik Resest At!
    volumes:
      - db_verileri:/var/lib/mysql # Databasedeki Kalici Disk Bolumu (Silinmesin Dİye)

# Ortak Kullanilacak HardDisk Hacimleri:
volumes:
  db_verileri: # Bos bir Objedir
```

Aynı Kodun (Üstünde oynama yapılamayan) JSON Karşılığında **Onlarca Kapatılmamış `{}` ve `"` yüzünden Sözdizimi Hatası (Syntax Error) yiyeceksinizdir**. YAML, gözü hiç yormadan insanın Bir metin belgesi Okur Gibi "Listeler" görmesini Sağlayan Okunabilirlik Ustasıdır.

## Kimler Kullanır?
* Evrendeki bütün **Sistem Yöneticileri (SysAdmin), DevOps Mühendisleri** ve Bulut (Cloud/AWS/GCP) Mimarları.
* Uygulamalarına Hızlıca (Programcı olmayan kullanıcıların bile kolayca Modifiye Edebileceği) Ayar dosyaları Kuran Oyun Sunucuları (Örn: Minecraft Server Configleri çoğu `config.yml` dir ve Çocuklar bile açıp Değiştirir).
* İnternet üzerinden Datayı yollarken (Cok yer kapladıgı ve Parse etmesi JSON'a gore agir oldugu icin) KULLANILMAZ. Yaml Arkaplanda Bekleyen Ayar dosyasıdır, Paketçi degildir.
