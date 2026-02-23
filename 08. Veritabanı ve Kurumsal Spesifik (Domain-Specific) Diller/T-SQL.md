# T-SQL (Transact-SQL)

## Özet
T-SQL (Transact-SQL); Oracle'ın (PL/SQL ile) veritabanı dünyasını domine etmesine karşılık olarak, Microsoft ve Sybase tarafından geliştirilen, gücünü **Microsoft SQL Server (MSSQL)** üzerinde gösteren, prosedürel mantık (IF/WHILE), lokal değişkenler ve pürüzsüz C/C++ kokan hata blokları ekleyen **Microsoft'un Tescilli (Proprietary) Prosedürel Veritabanı Dilidir**.

## Nedir ve Ne İşe Yarar?
Oracle'ın PL/SQL'i bir Pascal klonuyken, Microsoft veritabanı dünyasına hakim olmak için Sybase firmasıyla ortak bir "Dil Eklentisi" üretti. O da standart zayıf SQL'in sonuna "Transact (İşlem-Geçirimli)" kelimesini koyan T-SQL'di. 

Siz C#'ta (Web sitenizde veya Windows Form'unuzda) "Ahmet'ten Mehmet'e para transferi et" düğmesine bastığınızda; C# bunu veritabanına sadece "SQL Emri Atarak" geçirmez. Para yollama algoritmasını Database'in ta içine, MSSQL Sunucusundaki bir **T-SQL Saklı Prosedürüne (Stored Procedure)** fırlatırsınız. T-SQL Ahmet'in parasını çeker(Select), "EGER (IF)" yeterliyse çeker(Update), Hata çıkarsa(Try/Catch) işlemi Geri Alır (Rollback). Yani tam teşekküllü bir programı doğrudan HDD/RAM olan MS Sunucusunda yürütür.

**Ne İşe Yarar?**
* **Microsoft ASP.NET / C# Ekosisteminin Arkabahçesi:** Windows sunucularındaki web sitelerin arka planında koşturan (Örn: Hepsiburada, Yemeksepeti eski mimarisi gibi Windows ağırlıklı Devlerin/KOBİ'lerin) devasa E-Ticaret Stok Sepeti algoritmaları direkt T-SQL komut paketleriyle yazılmıştır.
* **Tetikleyiciler ve Zamanlanmış İşler (Jobs):** Gece saat 03:00'da "Sql Server Agent" uyandığında, verileri otomatik temizleyip yedeğini e-mail olarak yöneticiye atan komutlar (%100 T-SQL Döngüleridir).

## Dilin Mantığı ve Kod Yapısı
Sözdizimi olarak PL/SQL (Oracle)'den belirgin (Microsoft'a özel) estetik farklılıkları vardır. 
En belirgin özelliği BÜTÜN yerel (Local) Değişkenlerin isimlerinin **`@` (At / Kuyruklu A)** işaretiyle başlaması zorunluluğudur. Global/Sistem değişkenleri (Örn: `@@ERROR`) ise çift kuyruklu aa (`@@`) şeklindedir.

PL/SQL da For-Döngüsü varken, T-SQL çok minimalisttir ve sadece **`WHILE`** döngüsünü kullanır (Sınırlı kurgu, daha çok Set-Bazlı/Satır işlemci kafası savunur).

Yazılımcılar PL/SQL'in O `EXCEPTION` bloğunu değil; C#'in o efsanevi **`BEGIN TRY ... BEGIN CATCH`** yapısını MS-SQL T-SQL'ine entegre etmiş ve veritabanı hatasını C# formunda kurgulamışlardır.

**Örnek İşleyiş (Sembolik Olarak):**
Değişken Atama Oracle'da: `v_Yas NUMBER := 5;`
Değişken Atama T-SQL'de: `DECLARE @Yas INT = 5;` veya `SET @Yas = 5;` (Tam Microsoft formatıdır).

### Örnek Bir T-SQL Kodu: C# Güzelliğiyle Güvenli Para Transferi (Transaction / Try-Catch)
Binlerce işlemi tek bir Kapsül'e (Transaction) sarmalayan, hata olursa parayı havada Asılı bırakmayıp geriye vurduran o güvenli ve "Kuyruklu-a lı" Stored Procedure (Prosedür) Bloğu:

```tsql
/* T-SQL de Yorum Satiri Cift Tire (--) veya C tarzi /* */ ile cizilir */

-- "BankadanParaGonder" Isminde Kalici Bir Programlama Fonksiyonu Yarat:
-- İki tane "Disaridan Istek (Parametre)" Temsil Eden @ li Degisken koy
CREATE PROCEDURE ds_BankadanParaGonder
    @GonderenMusteriID INT,
    @AliciMusteriID INT,
    @YollanacakTutar DECIMAL(18, 2)
AS
BEGIN
    -- MS SQL Ayarı: "Bana etkilendi satirlari(1 row affected mesajlarini) GIZLE ki performans artsin!"
    SET NOCOUNT ON;
    
    -- YEREL DENETIM DEGISKENI YARATMA (DECLARE) 
    DECLARE @GonderenGuncelBakiye DECIMAL(18, 2);

    -- MODERN HATA YAKALAMA ZIRHI (C# ile ayni felsefe)
    BEGIN TRY
        
        -- ISLEM(TRANSACTION) MUCİZESİNİ BASLAT: 
        -- "Ya Hep Ya Hic" demektir. Yariya kadar islem yapip elektrik kesilirse iptal eder.
        BEGIN TRANSACTION ParaTransferiHarekati;

        -- 1. ADIM: Gonderenin Parasini Bul (Select ile degiskene aktar)
        SELECT @GonderenGuncelBakiye = Bakiye_Miktari 
        FROM BankaHesaplari 
        WHERE MusteriID = @GonderenMusteriID;

        -- 2. IF/ELSE SORGULAMASI (Para Yetiyor Mu?)
        IF (@GonderenGuncelBakiye < @YollanacakTutar)
        BEGIN
            -- RAISERROR: Sistemin disina (C#'a) Hata Firlat/Patlat 
            RAISERROR ('HATA: Musterinin Bakiyesi Yetersizdir. Islem Kesildi!', 16, 1);
            -- 'Return' komutuyla proseduru aninda terk et 
            RETURN;
        END

        -- EGER BAKIYE YETERLIYSE -> NORMAL YAZILIM MATEMATIGI:
        
        -- Gonderen Kisinin Anasindan Dus (Update!)
        UPDATE BankaHesaplari 
        SET Bakiye_Miktari = Bakiye_Miktari - @YollanacakTutar 
        WHERE MusteriID = @GonderenMusteriID;
        
        -- Aliciya Ekle
        UPDATE BankaHesaplari 
        SET Bakiye_Miktari = Bakiye_Miktari + @YollanacakTutar 
        WHERE MusteriID = @AliciMusteriID;

        -- HER SEY 10 NUMARAYSA IŞLEMI VE COMMIT(KAYDET)!
        COMMIT TRANSACTION ParaTransferiHarekati;
        
        -- Console'a(Messages) yazi firlat
        PRINT CSTR(@YollanacakTutar) + ' TL basariyla Havale Edildi.';

    END TRY
    
    
    -- EGER IF'TEKI RAISERROR TETIKLENIRSE VEYA SISTEM COKERSE : BURAYA DUS!
    BEGIN CATCH
        
        -- CATCH BLOGUNA DÜŞTÜYSE: İşlemi sonsuza kadar İptal et (Geri Makarasini Sar!)
        -- Bu sayede paranin yarisinin hesaptan dusup ötekine gitmeme Sacmaligi ÖNLENİR
        IF @@TRANCOUNT > 0
            ROLLBACK TRANSACTION ParaTransferiHarekati;
            
        -- Hatanin nedenini Log/Kayıt olarak ekrana veya tabloya bas
        DECLARE @HataNedeni NVARCHAR(4000) = ERROR_MESSAGE();
        PRINT 'TEHLİKE ENGELLENDI: Çökme Nedeni -> ' + @HataNedeni;
        
    END CATCH

END
```
T-SQL Dünyası; Satır-satır (Cursor) döngülerinden çok nefret eder. Eğer 50 milyon kişiyi `WHILE` döngüsüyle T-SQL kullanarak taramaya kalkarsanız Microsoft SQL Sunucunuz yanar (Yüzlerce saniye sürer). Bunun nedeni; MSSQL'in "Kümelerle (Set-Based)" (UPDATE Users SET maas = maas * 1.5) ile blok olarak çok hızlı çalışmasına göre modellenmesidir. T-SQL Coder'ı; Döngülerden tamamen kaçan ve "Tek emirlik Kümelere/Update'lere" sığınan bir Cerrahtır (Aksi halde veritabanını felç eder).

## Kimler Kullanır?
* Evrendeki bütün şirketler ağı olan .NET ve Microsoft Kurumsal geliştiricilerinin (Windows Server, IIS, C# MVC siteleri) ana Veritabanı Yöneticileri **(SQL Server DBA = Database Administrator)**.
* Büyük şirketlerin Muhasebesini yürüten ERP programcıları (Micro, Logon vb) sürekli T-SQL scriptleriyle Rapor/View komutları koşturur. Lakin Oracle ve PL/SQL'e göre kullanımı daha Kolay ve Visual Studio (.NET SSMS) arayüzleriyle sarmalanmış olduğundan dünyada daha yaygındır.
