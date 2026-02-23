# PL/I

## Özet
PL/I (Programming Language One); 1964 yılında dev tekelci IBM tarafından, zamanın paramparça olmuş "İş dünyası (COBOL)" ve "Bilim Dünyası (Fortran)" kodlama kamplarını birleştirip **"Her Şeyi Yapabilen Tek Bir İsviçre Çakısı"** efsane dil kurmak amacıyla agresif ve çılgınca tasarlanan, devasa karmaşıklıktaki anıtsal programlama dilidir.

## Nedir ve Ne İşe Yarar?
1960'larda Bilgisayar biliminde keskin bir ırk ayrımı vardı: Şirket muhasebecileri Amerikan Dolarını hesaplamak için COBOL dili kullanıyordu, Akademik bilim insanları füzelerin rotasını hesaplamak için Fortran kullanıyordu. İkisi bir araya gelemiyor, aynı bilgisayar sisteminde bile donanım kavgaları yaşanıyordu.

IBM "O zaman herkes sussun, biz öyle devasa bir dil yapacağız ki; hem COBOL'un banka/string gücünü alacak, hem Fortran'ın matriks/matematik hızını alacak, hem de yeni nesil ALGOL'un süslü blok(begin..end) yapılarını kapacak!" diyerek PL/I dilini yarattı. Ve gerçekten "Her Şeyi Yapan Dil" oldu!

**Ne İşe Yarar?**
* **IBM Mainframe Altyapısı (Z/OS vb):** Milyarlarca dolarlık banka sunucularında, Amerikan vergi dairesinin ana yığma kayıtlarında COBOL'un tek alternatifi olan kurumsal ve eşzamanlı bir programcılık zırhıdır.
* **Gelişmiş Veri Yapıları:** PL/I; O yıllar için çok korkunç ve büyük bir teknoloji olan *Pointer (İşaretçi)*, *Bit Strings (10111 vb Bit Metinleri)* ve dinamik Hafıza Yönetimi (`ALLOCATE`, `FREE`) komutlarını bünyesinde aynı anda taşıyabilen ilk Multi-Paradigma dillerdendi.

## Dilin Mantığı ve Kod Yapısı
Bu dilin en büyük Eleştirisi (Tarihi kusuru) **Aşırı Karmaşıklık (Bloated Language)** idi. Eğer bir dil hem Veritabanı okuyup, hem Roket yörüngesi hesaplayıp hem de string düzenleyecekse, binlerce farklı Anahtar Kelime ve Kural'a muhtaçtı. PL/I kitapları adeta 2 ansiklopedi boyutundaydı (Dili tam öğrenen bir IBM mühendisi dahi nadirdi).

"Sıfır Rezervasyon Karakter" kuralı vardır! Çok tuhaftır: PL/I dilinde `IF` sadece bir kelimedir; isterseniz `IF` isminde bir değişken oluşturup sonra formül olarak `IF IF = THEN THEN ELSE = IF` ("Eğer IF isimli değişken THEN ise...") yazıp derleyiciyi şizofreniye sokabilirdiniz ve sistem hata vermez çalışırdı!

**Örnek İşleyiş (Sembolik Olarak):**
Prosedürel (İşlemsel) bir dildir fakat Hata Yakalama (Exception Handling) denen ve şimdiki Try/Catch bloklarına denk düşen "ON CONDITION" kalkanını modern dünyadan çok evvel muhteşemce kurgulamıştır.

### Örnek Bir PL/I Kodu: "Her Şeyin Dili" Blok Yapısı ve Hata Avcılığı
IBM'in 360 Serisi devasa bilgisayarlarında Fortran'ın matematik dizilerini COBOL'un veri bloklarıyla bir arada yürüten o devasa sözdizimi:

```pli
/* PL/I dilinde yorumlar C/C++ in alacagi gibi islenir: /* yorum */ */

HELLO: PROCEDURE OPTIONS(MAIN); 

  /* Hem Degisken (DECLARE/DCL), hem tip (FIXED/FLOAT) hem Karakter blokları birarada: */
  DCL ISIM CHAR(20) INITIAL('IBM MUHENDISI');
  DCL BAKIYE FIXED DEC(10,2) INIT(10050.75); /* Kuruşu kurşuna COBOL tarzi kesin virgüllü Tip! */
  
  DCL I FIXED BIN(31); /* Normal bir tam sayi indexi */

  /* STRING MANIPULASYONU (Fortran'in yapamadığı sey) */
  PUT SKIP LIST('HOSGELDIN, ' || ISIM); /* || isareti Stringleri birlestirir! */
  
  PUT SKIP LIST('--- FATURA VE HESAP SISTEMI ---');

  /* KUSURSUZ (EXCEPTION) HATA YAKALAMA MIMARISI (ON X) */
  /* Eğer asagida sifira bolme ('ZERODIVIDE') hatasi yaparsan, cökmeden bu blogu aninda islet! */
  ON ZERODIVIDE 
  BEGIN;
    PUT SKIP LIST('KRIZ!: Bir sayi sifira bolunmeye calisildi! Bakiye 0 yapiliyor.');
    BAKIYE = 0;
    GOTO END_ISLEM; /* Goto ile felaketten kaciyoruz */
  END;

  /* Hata simülasyonu yapan tehlikeli Dongu */
  DO I = 5 TO 0 BY -1; /* Geriye dogru say */
      PUT SKIP LIST('Sayac: ', I, ' Isleniyor...');
      
      /* Eger Sayac SIFIR olursa, Sistem SIFIRA BÖLÜNME(ZERODIVIDE) hatasina tetiklenecek 
         ve yukaridaki 'ON ZERODIVIDE' kalkanina carparak kurtarilacak! */
      BAKIYE = BAKIYE / I;
  END;

END_ISLEM:
  PUT SKIP LIST('Guvenli Sistem Cikisi.');
  PUT SKIP LIST('Musterinin Kalan Bakiyesi: ', BAKIYE);

END HELLO;
```
1960'larda SIFIRA BÖLÜNME esnasında Koskoca Mainframe (Anaçatı) bilgisayar komple Çöker ve kilitlenirdi, PL/I bu `ON CONDITION` efsanesiyle modern Error/Catch(Hata Yakalama) teorisini sağlamlaştırdı.

## Kimler Kullanır?
* Evvelden kalma çok eskimiş ve yığma (Legacy) Amerikan uçuş bilet kayıt sistemlerini ve uluslararası vize başvuru IBM bankalarını (Eğer COBOL ağırlıklı değilse) ayakta tutan Niş ve Elit yaşlı mühendisler tabakası.
* Günümüzde yeni hiçbir projede bu dil kullanılmaz, hantallığı ve C/Java'nın açık kaynak özgürlük devrimi, IBM'in kapalı kutu kurumsal PL/I tekelini tahtından on yıllar önce etmiştir.
