# PL/SQL

## Özet
PL/SQL (Procedural Language extensions to SQL); 1990'ların başında Oracle Corporation tarafından, dümdüz ve sadece "Bana veri getir/sil" diyebilen standart SQL'in o zayıf komut yapısını yıkmak için icat edilen; içine **Değişkenler, İf/Else, For-Döngüleri ve Hata-Yakalamayı (Exceptions)** yerleştirerek Oracle Veritabanı içinde saniyede milyonlarca hesabı C/Java hızıyla çalıştıran devasa bir **Prosedürel Veritabanı Dilidir**.

## Nedir ve Ne İşe Yarar?
Standart SQL (Level 7 başındaki SQL e bakınız) "Bildirimseldir (Declarative)". Sadece ne istediğinizi söylersiniz: `SELECT * FROM İsciler WHERE maas > 5000`. Ancak SQL ile "*Eğer* işçinin maaşı 5000'den büyükse onu ayrı bir tabloya kopyala, oradaki vergiyi %10 yap ve her müşteriye mail atan fonksiyonu tetikle" (IF/ELSE DÖNGÜSÜ) yazamazsınız. SQL bir yazılım dili değil, sorgu dilidir.

İşte Oracle, veritabanından Java'ya veya Python'a veri çekip yavaşça yormak (Network Gecikmesi) yerine; "Neden Algoritmayı(If-Else/For) Veritabanının **KALBİNE** gömmüyoruz ki?" dedi ve **PL/SQL**'i çıkarttı. Tarihi Pascal ve Ada dillerinden ilham alan bir syntax ile (begin..end bloklarıyla) Veritabanı motorunun ta içinde program yazdırmaya başladı!

**Ne İşe Yarar?**
* **Triggers (Tetikleyiciler):** Dünyadaki her banka ve telekomünikasyonda; Veritabanına (Örn "Müşterinin Bakiyesinden 100 TL düştü") emri SQL'den geldiği saniye, PL/SQL tetikleyicisi uyanır ve o 100 TL'nin log kaydını tutar, bakiyesi eksiye düşerse işlemi iptal (Rollback) eder; Tüm bu algoritma Java/C# uygulamasına bile gitmeden Veritabanında (Oracle'da) milisaniyede bitirilir.
* **Stored Procedures (Saklı Prosedürler):** Kod, uygulamanın (Web Sitenizin) içinde değil, veri bankasında `CREATE PROCEDURE` olarak durur. Web siteniz sadece "Hesapla()" emri yollar, milyonlarca satırlık veritabanı kendi içinde kaynayıp sonucu web sitesine dümdüz 1 rakam olarak döndürerek Server'ı (Ağı) asla yormaz.

## Dilin Mantığı ve Kod Yapısı
Tam bir **Ada / Pascal** klonudur. Süslü Parantez `{}` asla göremezsiniz. Klasik "Blok Yapılıdır".
Her PL/SQL kodu zorunlu **3 bloktan** birine uymalıdır:
1. `DECLARE`: Değişkenlerin tanımlandığı bölge (Zorunlu değil).
2. `BEGIN...END;`: Kodun(Döngülerin) asıl çalıştığı kalbidir.
3. `EXCEPTION`: Eğer sıfıra bölünme veya verinin bulunamaması (NO_DATA_FOUND) gibi bir felaket olursa sistemin çökmesini engelleyen kurtarma kalkanı.

Değişken atamaları Pascal'daki gibi `:=(İki nokta üst üste eşittir)` ile yapılır. Satırlar noktalı virgülle biter. Ek olarak, içine dümdüz (Native) SQL satırlarını yazıp onu değişkenlere `INTO` kelimesiyle aktarabilirsiniz.

**Örnek İşleyiş (Sembolik Olarak):**
Java: `int yas = 25; if (yas>18) { db.sil(id); }`
PL/SQL: `v_yas NUMBER := 25; BEGIN IF v_yas > 18 THEN DELETE FROM isciler; END IF; END;` (Bunu doğrudan Oracle Konsoluna/SQL Geliştiricisine yazarsınız).

### Örnek Bir PL/SQL Kodu: Cursor (İmleç) ile Veritabanı Döngüsü Süpürgesi
Milyonluk tablodaki tüm müşterileri tek tek For Döngüsüne (Cursor) alıp, Maaşına göre arkaplanda "Hata Çökmelerini de önleyerek" otomatik prim veren prosedür!

```sql
/* PL/SQL dilinde yorumlar C-Tipi veya Cift-Tire (--) ile yapilir. */
-- Oracle Veritabanina Ozel Bir PL/SQL(Prosedur) Blok!

DECLARE
    -- 1. DEGISKEN YARATMA: "Dinamik Tip Çekme" Mucizesi!
    -- v_maas'in tipi, "Musteriler Tablosundaki MAAS Sütunun Tipi neyse O olsun(%TYPE)" 
    v_maas Musteriler.MAAS%TYPE; 
    v_prim_miktari NUMBER(10, 2); -- 10 Haneli, 2 Kuruslu Sayi
    
    -- 2. CURSOR MUCİZESİ (Satır Satır Okuma Motoru):
    -- Select komutunun ucuna 'c_musteriler' isminde Vagon bagla:
    CURSOR c_musteriler IS
        SELECT MUSTERI_ID, MAAS FROM Musteriler WHERE DURUM = 'AKTIF';

BEGIN
     -- Ekrana (Console'a) Çıktı(Print) Basma Komutu: DBMS_OUTPUT pakedi!
     DBMS_OUTPUT.PUT_LINE('--- Yil Sonu Prim Dagitimi Basliyor ---');

     -- O MESHUR PL/SQL FOR DÖNGÜSÜ:
     -- Cursor'ün(Sorgunun) ucundaki her Bir Satır (v_kayit) icine Gir ve Döngüle:
     FOR v_kayit IN c_musteriler LOOP
     
        -- Eger Maaş 10 Bin den azsa, Primi %10 (0.10) ver. Veya (ELSIF) %5 ver!
        IF v_kayit.MAAS < 10000 THEN
            v_prim_miktari := v_kayit.MAAS * 0.10;
        ELSIF v_kayit.MAAS >= 10000 AND v_kayit.MAAS < 20000 THEN
            v_prim_miktari := v_kayit.MAAS * 0.05;
        ELSE
            v_prim_miktari := 0; -- Zenginlere prim yok
        END IF;

        -- HESAPLANAN PRIMI DATABASE'e GERCEKTAN YAZ (UPDATE):
        UPDATE Musteriler 
        SET PRIM = v_prim_miktari
        WHERE MUSTERI_ID = v_kayit.MUSTERI_ID;
        
     END LOOP;
     
     -- Döngü Bitti, Her şey yolunda ise Veritabanında Yapilanlari Onayla (COMMIT-KAYDET)
     COMMIT;
     DBMS_OUTPUT.PUT_LINE('Islem Hatasiz Bitti!');

-- 3. EXCEPTION (HATA) BLOGU: Eger ustteki UPDATE'de fissek cekerse ne olur?
EXCEPTION
    WHEN NO_DATA_FOUND THEN
        DBMS_OUTPUT.PUT_LINE('KRITIK HATA: Aktif Musteri Bulunamadi.');
    
    WHEN OTHERS THEN
        DBMS_OUTPUT.PUT_LINE('Sistem Hatasi Olustu, Islemler İptal Ediliyor.');
        -- Olasi bir yarıda kesilmede, Database'i bozmamak icin her şeyi Sifirla (GERI AL):
        ROLLBACK;
        
END;
-- '/' Isareti Oracle'a "Blogun Derlemesini Bitir ve Calistir" demektir
/ 
```

C# EntityFramework'ün ya da Node.js Sequelize'ın veritabanından 1 Milyon satırı "Ağ üstünden/TCP" Web Sunucusuna (RAM'e) çekene kadar Ağlatacağı o yavaşlığı; PL/SQL veritabanı diskinin kendi RAM'i içinde Pürüzsüz Döngüleyerek Işık hızında bitirir.

## Kimler Kullanır?
* Finans, Telekom, Sigorta veya Havayolu rezervasyonu yapan devasa kurumlarda mimarinin tam kalbini (%100 Oracle Veritabanlarını) tutan **Oracle DB Admin (DBA) ve PL/SQL Geliştiricileri**.
* Oracle (Larry Ellison) devine Milyonlarca dolar lisans parası ödeyen Bankaların arka-plan kilit programcılarıdır.
* Sadece ve sadece Oracle veritabanlarında çalışır! (Açık kaynak dünyasındaki **PostgreSQL**, bunun klonu olan `PL/pgSQL` dilini icat etmiştir. O da tamamen aynı felsefedir lakin kütüphaneleri C kökenlidir).
