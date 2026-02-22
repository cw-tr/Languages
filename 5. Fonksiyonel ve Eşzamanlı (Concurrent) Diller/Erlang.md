# Erlang

## Özet
Erlang, 1986'da Ericsson şirketinin "Kırılması/Çökmesi İmkânsız" devasa telefon santrallerini (telekomünikasyon altyapılarını) kesintisiz yönetmesi için laboratuvarlarda yaratılan, çok benzersiz ve efsanevi **Hata-Toleranslı (Fault-Tolerant) Eşzamanlı (Concurrent)** fonksiyonel programlama dilidir.

## Nedir ve Ne İşe Yarar?
1980'lerde eğer Avrupa'daki ana dağıtım sunucusunun C diliyle yazılmış kodunda ufak bir Pointer hatası çıksaydı, sunucu 1 saniyeliğine reset atar ve o reset atılırken Avrupa'da telefonla konuşan 1 Milyon insanın aynı anda santral hattı yüzüne kapanırdı. Bu kabul edilemez (Nine Nines - %99.9999999 Kesintisiz Uptime garantisi istenen) dev bir sorundu.

Joe Armstrong, Erlang dilini yepyeni bir "Sanal Makine (BEAM)" ile yarattı. Bu makinedeki vizyon şuydu: **"Bırakın sistem Çöksün! (Let it crash!) Ama çöken küçük parça sadece kendini yok etsin, yerine hemen klonu uyansın, ana sistem asla hissetmesin!"**

**Ne İşe Yarar?**
* **Milyonlarca Anlık Bağlantı:** Klasik Apache (PHP/Java) web sunucuları bilgisayarın içine 10 Bin işlem düğümü (Thread) verince kitlenir, RAM patlar. Erlang BEAM makinesi kendi ufacık ağırlıksız işçilerini (Proses/Aktör) yaratır ve tek bir laptop dahi aynı milisaniyede 20-30 Milyon farklı aboneye/mesajlaşmaya dondurmadan (Lag olmadan) yanıt verebilir.
* **Canlı Kod Değiştirme (Hot Code Swapping):** Erlang'ın mucizesidir. Bir şirketin veritabanı açık ve saniyede milyonlarca para akarken, siz yeni yazdığınız bir Erlang fonksiyonunu (.exe versiyonunu) sistemi **kapatmadan, yeniden başlatmadan ve kimseyi hatta atmadan** uçaktayken havadayken motor değiştirmek gibi sisteme canlı gömüp yoluna sorunsuz devam ettirebilirsiniz.

## Dilin Mantığı ve Kod Yapısı
Felsefik olarak Scala ve Haskell gibi **Fonksiyoneldir**; yine değişkenlerin içi, atandıktan sonra bir daha bozup değiştirilemez (Immutable). Veri kilitlenmez (Data Lock/Mutex yoktur).

Erlang dilinde binlerce objeniz (`class`) değil, yüzbinlerce ayrı yaşayan küçük böcekler/canlılar (**İşlemler - Process / Aktör Mimari**) vardır. Bu canlı hücreler birbirinin hafızasına DOKUNAMAZ (Strict Isolation). Sadece birbirlerinin Posta Kutularına (Mailbox) mektup fırlatıp kaçarlar. ("Mesaj Gönderme - Message Passing"). Biri mektubu işlerken hata (ZeroDivision vb.) alırsa ve acı içinde ölürse, tepesinde duran süpervizör hücre onun (şehit düştüğünü) saniyesinde görüp yepyeni hatasız kopyasını uyandırır. Kullanıcı hiçbir şey hissetmez.

**Örnek İşleyiş (Sembolik Olarak):**
Milyarlarca kişinin girdiği devasa (WhatsApp benzeri) "Chat" uygulamasında; Erlang o odaya giren herkesi tek bir dev havuza itmek yerine, Ayşe'nin süreci, Mehmet'in süreci diyerek 1 Milyon birbirinden bağımsız ağırlıksız baloncuk açar. Mehmet baloncuğunu kod hatasıyla çökertirse, Ayşe sohbetine (Sistemde) hiçbir şey olmamış gibi zerre yavaşlamadan 60 PFS devam eder.

### Örnek Bir Erlang Kodu: Mesajlaşma (Posta Kutusu ve Aktörler)
Tam bir telekomünikasyon mantığı ile iki ayrı canlı hücrenin (Process) uyanıp, birbirlerinin posta kutularına ( ! sembolü) veri fırlattığı gerçekçi (Pattern-Match) örneği:

```erlang
% Erlang'da Yüzde isareti (%) Yorum satırı anlamına gelir.

-module(pingpong).          % Kutusunda izole duran Modül kuralı
-export([start/0, ping/2, pong/0]). % Dış dünyanın (diğer aktörlerin) görebileceği Komutları deklare et!

% Pong (Karşılayan Sunucu) Hücresi/Aktörü
pong() ->
    receive  % Posta Kutusunda(Mailbox) "Mektup BEKLE" emri! (Bloke olup uyur)
    
        % Eger mektubun uzerinde PING yaziyorsa ve icinde donulecek 'Ping_Adres_Hucresi' yaziliysa bunu yap:
        {ping, Ping_Adres_Hucresi} ->
            io:format("PONG Hucresi: 'Ping' mektubunu aldim! Geri Pong Firlatiyorum...~n"),
            
            % Mucize '!' (Ünlem Fırlatma) İslemi: Ping Hucresinin Kutusuna 'pong' at. Asenkron! (Aninda kac).
            Ping_Adres_Hucresi ! pong,
            
            % Geri dön (döngüyü sürdür, sonsuza dek uyanıp bekle)
            pong();
            
        % Eger mektubun uzerinde BİTİR(finished) formati varsa hücresel olarak İntihar/Sonlanma gerceklesir.
        finished ->
            io:format("PONG Hucresi: Elvada dunya, yorulup isimi bitiriyorum!~n")
    end.


% Ping (Gönderen Müşteri) Hücresi/Aktörü
ping(0, Pong_Adres_Hucresi) ->
    % Eger deneme şarijimiz bittiyse (0 olduysa) Mektuba "Bitir" yazip Karşı Tarafa fırlat (Kapat).
    Pong_Adres_Hucresi ! finished,
    io:format("PING Hucresi: Isimi bitirdim ve kapandim.~n");

ping(Tekrar_Sayisi, Pong_Adres_Hucresi) ->
    % Karsi pong hucresine ({ping, BenimGercekAdresim(self())} paketi yollayip kac).
    Pong_Adres_Hucresi ! {ping, self()},
    
    receive
        pong ->
            io:format("PING Hucresi: Karsidan basarili 'Pong' cevabi geldi!~n"),
            % Donguye giristirip (-1) bir sayac daha gonderir!
            ping(Tekrar_Sayisi - 1, Pong_Adres_Hucresi)
    end.

% ANA TETİKLEYİCİ (Uygulamayi baslat):
start() ->
    % 'spawn' (Yumurtla/Üret) komutu BEAM sanal makinesinde yepyeni, tamamen izole agirliksiz bi islemci(Aktör) uyandirir.
    Pong_Hucresinin_PIDsi = spawn(pingpong, pong, []),
    
    % Asil ping isini (3 tur pinpon oynayacak sekilde) yine yep yeni bi izole islemciyle baslat!
    spawn(pingpong, ping, [3, Pong_Hucresinin_PIDsi]).
```

## Kimler Kullanır?
* **WhatsApp (Meta):** Efsanevi başarı örneğidir. Dünya üzerinde (başlangıç döneminde) sadece 50 civarı mühendis ile milyarlarca mesajın milisaniyede çökelmeden, lagsız, kesintisiz okyanusları aşarak insanları 60 FPS bağlaması, altyapısını %100 saf Erlang ile örmesi sayesinde (Ejabberd XMPP) olmuştur.
* Mobil Operatör Santralleri (LTE / 4G Servisleri / Cisco Router Cihazları ve Ericsson baz istasyon kalpleri).
* Discord Sesli Arama Sunucuları (Voice Chat), Riot Games mesajlaşma ve eşleştirme motoru gibi arka planın "Asla Çökmemesi" gereken anlık canlı (Real-Time) ağ mimarları.
