# SQL

## Özet
SQL (Structured Query Language - Yapısal Sorgulama Dili); 1970'lerde IBM tarafından icat edilen, yazılım dünyasındaki geleneksel programlama kalıplarının tamamen dışında kurgulanmış, yeryüzündeki tüm "İlişkisel Veritabanlarını (RDBMS)" yönetmek, sorgulamak ve manipüle etmek için kullanılan tartışmasız endüstri standardı **bildirimsel (declarative)** spesifik altyapı dilidir.

## Nedir ve Ne İşe Yarar?
Eğer elinizde 1 Milyon insanın "Adı, Soyadı, Kredi Kartı Numarası ve Adresi" varsa, bunu normal düz bir metin (.txt) veya Excel dosyasında saklamak bilgisayar RAM'ini patlatır ve aramalarda saniyeler/dakikalar çalar. Veriler; "Tablolar" halinde, birbirine iplerle bağlı (İlişkisel - Relational) güvenli devasa mahzenlerde (Veritabanları) tutulur. MySQL, PostgreSQL, Oracle, Microsoft SQL Server bu mahzenlerin genel isimleridir.

İşte **SQL**, bu devasa mahzenlerle konuşabildiğimiz evrensel tek dildir.

**Ne İşe Yarar?**
* **Veri Çekme ve Yönetme (CRUD):** Bir E-Ticaret sitesinde "Sadece kırmızı renkte olan, fiyatı 500 TL'nin altındaki ve stokta kalmış ürünleri" 10 mili-saniyede bulup size getirmesini SQL diliyle Veritabanı motoruna emredersiniz. C veya Java üzerinden de olsanız, o kodun içinde motora SQL Cümlesi (Query) göndermek zorundasınızdır.
* **Analiz ve Raporlama (Data Analysis):** Bankacılık sistemlerinde "Son 1 ayda maaşı 20 bin TL üzeri olup en çok harcama yapan top 10 müşteri" gibi çıldırtıcı analitik istatistikleri, Veri Analistleri sadece SQL metinleriyle süzer.

## Dilin Mantığı ve Kod Yapısı
SQL, geleneksel programlamadaki gibi "Şu döngüyü kur, X değişkenini arttır, eğer buysa böyle yap" (Imperative/Buyurucu) mantığı ile ÇALIŞMAZ. Mükemmel bir İngilizce cümlesidir: **Siz sadece NEYİ istediğinizi söylersiniz, Veritabanı motoru onu NASIL getireceğini kendi optimize eder.**

Dört ana temel yapı sütunundan oluşur:
1. `SELECT`: Mahzenden Oku / Getir (Read)
2. `INSERT`: Mahzene Yeni Veri Koy (Create)
3. `UPDATE`: Mahzendeki Eski Veriyi Değiştir (Update)
4. `DELETE`: Mahzendeki Veriyi Yak / Sil (Delete)

**Örnek İşleyiş (Sembolik Olarak):**
Python'da olsaydı satır satır dosyayı açar, bir `for` döngüsüyle her satırda "Fiyat 500'den küçük mü?" diye `if` kontrolü atardınız (Çok yavaş). SQL'de bunu düz bir emir kipiyle `SELECT * FROM İlanlar WHERE Fiyat < 500` diyerek yazarsanız, C ile yazılmış olan Veritabanı Motoru "B-Tree İndex" mimarisiyle o satırları sizin için anında süzüp getirir.

### Örnek Bir SQL Kodu: Sorgu (Query) Sanatı
Bir şirketin yüz binlerce çalışan verisini tutan tablosundan, saniyeler içinde zekice filtrelenmiş veriyi alan klasik ve muazzam bir Raporlama Sorgusu:

```sql
-- SQL dilinde Yorum Satırları çift tire (--) ile gösterilir.
-- SQL kodları genellikle BÜYÜK HARFLERLE (Standart) yazılır, okunabilirliği artırır.

-- 1. ADIM: "Bana hangi verileri istiyorsun?" diye sorar motor. 
-- "Calisanlar tablosundan Isim ve Maasi getir, Departmanlar tablosundan ise Departman Adini getir" diyoruz.
SELECT 
    Calisanlar.Isim, 
    Calisanlar.Soyisim, 
    Departmanlar.DepartmanAdi, 
    Calisanlar.Maas
    
-- 2. ADIM: "Bu verileri HANGİ (FROM) ana tablodan okumaya başlayayım?"
FROM 
    Calisanlar 
    
-- 3. ADIM (İLİŞKİSEL MUCİZE - JOIN): 
-- Çalışanlar tablosundaki departman ID'si ile Departmanlar tablosundaki gerçek odayı birleştir/dik!
INNER JOIN 
    Departmanlar ON Calisanlar.Departman_ID = Departmanlar.ID
    
-- 4. ADIM (FİLTRE - WHERE):
-- Herkesi getirme! Sadece "Muhendislik" departmaninda çalışanları, 
-- VE (AND) maaşı 50.000 TL'nin ÜZERİNDE olanları süz.
WHERE 
    Departmanlar.DepartmanAdi = 'Muhendislik' 
    AND Calisanlar.Maas > 50000
    
-- 5. ADIM (SIRALAMA - ORDER BY): 
-- Bulduğun bu sonuçları ekrana rastgele değil, Maaşı En Yüksek Olana doğru (DESC = Descending / Azalarak) sırala.
ORDER BY 
    Calisanlar.Maas DESC;
```
İşte bu saf metin bloku bir uygulama içinden veritabanına ulaştığında, motor bu emri alır ve donanım limitlerini zorlayan arka plan C algoritmalarıyla size tek bir sanal Excel tablosu olarak filtrenin yanıtını kusursuz döner. 

## Kimler Kullanır?
* Hemen hemen var olan **Bütün Yazılımcılar (Backend / Full Stack)**. Web sitenizin veya uygulamanızın veritabanı varsa arka planda mecburen ORM araçlarıyla veya saf metinlerle SQL ateşlersiniz.
* **Veri Analistleri (Data Analysts) ve İş Zekası (BI) Uzmanları:** Kod/Uygulama yazmayı hiç bilmeseler bile, sadece SQL dilini mükemmel seviyede bilerek şirketlerin dev veritabanlarına bu sorguları atıp Rapor oluştururlar (Metabase, Tableau gibi araçlar ile).
* Veritabanı Yöneticileri (DBA - Database Administrator); Sunucu yönetimini endeksleyen ve optimize eden altyapı mühendisleri. 
