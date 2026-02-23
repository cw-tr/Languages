# Ansible (Playbooks)

## Özet
Ansible; 2012 yılında Michael DeHaan tarafından geliştirilen ve sonrasında dev açık kaynak firması Red Hat (IBM) tarafından satın alınan; Şirketlerin veya Kurumların sahip olduğu "Yüzlerce Hatta Binlerce Tane Sunucuyu (Bilgisayarı)" tek tek el ile açıp içine girerek komut yazmak eziyetinden kurtarıp; **Tek Bir Tuşla ve Ajansız (Ajan Kurulumu Gerektirmeden)** Aynı Anda Tüm Milyonlarca Cihaza Uzaktan Program Yükleyen, format atan veya Ayar Yapan Evrensel "Sistem Konfigürasyon Otomasyon Aracıdır."

## Nedir ve Ne İşe Yarar?
Eskiden SysAdmin(Sistem yöneticileri) şöyle çalışırdı: Kredi kartı işlem altyapısı kuran C bankasının Elinde 100 Tane Linux Cihaz (Sunucu) var. Patron dedi ki: "Bütün Bu 100 makineye Acilen Güvenlik Yaması(Antivirüs/Patch) At!". 
Zavallı Sistem Yöneticisi saatler boyunca Putty (SSH) Üzerinden 100 Makineye Tek Tek Şifreleriyle Bağlanıp 100 kere `apt-get install yama` yazmak zorundaydı. Biri çöker, Birinin Bağlantısı Düştü mü Karmaşa çıkardı.

Ansible Dedi ki: Bana Bir **YAML formatında OyunKitabı (Playbook)** ver. Makine Liste(İp Adreslerini - Inventory) ver. Sonra Tıkla! Parola vs Girerek Arka Planda Ben(Ansible) Cihazlara "Gönüllü ajan(SSH)" olarak Sızıp Hepsine O 1 Komutu ayna Anada Senkronize Şuttlayacağım.

**Ne İşe Yarar?**
* **Massive Server Management (Kitlesel Sunucu Yönetimi):** Binlerce sunucu üzerinde Aynı Anda MySQL güncellemesi yapmak, Aynı Anda yeni Kullanıcı Şifreleri atamak sadece 1 Satırlık bir Ansible Konfigürasyonuna Bakar!
* **Agentless (Ajansız Mimarisi):** Ansible'ın Dünya Çapında Piyasayı(Eski rakipleri Chef ve Puppet'i) Ezmesinin Ana sebebi Cihazlara "Ben Ansible'ım Ajanımı kurun" Dememesidir. Çok İlkeldir, Doğrudan cihazların Saf (SSH) Portundan Kimlik göstererek sızar. Güvenlik Uzmanları Buna Bayılır.

## Dilin Mantığı ve Kod Yapısı
Ansible tamamen **YAML (Boşluk/Sözlük Formatı)** dilinde Yazılır. 
Ansible dosyalarına **"Playbook (Oyun Kitabı / Taktik Tahtası)"** denir. Tıpkı Basketbol maçında "Şimdi siz savunma, sen hücum" der gibi komutları belirler.

En Kritik iki Taşı (Mantığı) şunlardır:
1. **Inventory (Envanter) Dosyası:** Operasyonu Yiyecek olan (Hedef Kitle) Sunucu IP Numaraları Listesidir.
2. **Tasks (Görevler):** O Hedefler Ne(Hangi işlevi / Modülü) Yapacaktır? (Örn Yükle, Sil, İndir, Git-Clone Çek)

### Örnek Bir Ansible Mimarisi (Playbook - YAML): 1000 Bilgisayara Aynı Anda Apache (Web Sunucusu) Kurmak!
Ansible Terminalden Tetiklendiginde, Hedef Gruptaki (Adı 'web_sunucularim' Olan Tum Cihazlara) Saldiri ve Kurulum Planını uygulayacak O YAML Senaryo(Script) Kodu :

```yaml
# BU BIR ANSIBLE PLAYBOOK (Oyun Kitabi Taktigi) DOSYASIDIR (OrnekPlay.yml)

# 1. TEMEL KIMLIK VE HEDEF 
- name: Bütün Web Sunucularima (1000 Tane Cihaza) Apache Yükleme Operasyonu
  hosts: web_sunucularim      # Inventory (Ip listesi) dosyasinda Bu isimli Grubu Hedef Al!
  become: yes                 # Root (SuperUser / Mutlak İlah) Yetkisiyle Islem Yap! (Sudo yetkisi al)

  # 2. GOREVLER LİSTESI (TASKS) - SİRayla Hepsi Yapılır
  tasks:
    
    # GOREV 1: Paketin Yüklenmesi 
    # Bu Bir Aciklama(Name) dir.
    - name: 1. İslem -> Apache (Httpd) Paketnamesinin İndirildiğinden Emin Ol !! 
      dnf:                    # Linux'un DNF/YUM Paket Yoneticisi Modulune Baglaniyoruz!
        name: httpd           # İndirilecek Paketin Adi!
        state: latest         # Her zaman En Guncel Versiyonunu (Latest) Cek!
        
        
    # GOREV 2: Servisi Ayaga Kaldirma (Baslatma)
    - name: 2. Islem ->  Apache Servisini Tamamen Actigindan / Oynattignindan Emin OL!
      service:                # Ansible'daki Arkaplan (Daemon/Services) Modulu 
        name: httpd
        state: started        # Calistir(Start)!
        enabled: yes          # Cihaz Çöküp Reset atilirsa Tekrar otomatik Baslasin mi? YES!
        
        
    # GOREV 3: Web Sitesi İcerigine Benim Ozel Dosyami Atmak (Kopyalama)
    - name: 3. Islem -> Sitenin Ana Ekranına(Index.html'e) Ozel Bir Hosgeldin Yazisi Kopyala Gonder!
      copy:                   # Bilgisayatlar arasi Kopyalama Modülü
        content: "<h2>Ansible Tarafından Butun Dunyayya Isik Hizinda Automatik Gonderildi!</h2>"
        dest: /var/www/html/index.html   # Adamin Makinesindeki (Hedef Cihaz) Gideceği Klasor Yolu!
```

Eğer bir Mühendis Bu Dosyayı `ansible-playbook OrnekPlay.yml` Yazarak Fırlatırsa; Ekranda Yeşil ve Kırmızı yazılar Şelale gibi Akmaya (Logs) başlar. Kaç cihazın başarıyla Kurulduğu (`ok = 100`, `changed = 100`) Canlı olarak Gösterilir. Hata çıkaran Cihazlar Atlanır Kırmızıya Boyanır.  Eğer Sistem yöneticisi Çayını Tazeleyip Geldiğinde, Bütün Operasyon Mükemmel Bir Mutabakatla bitmiş Olacaktır!

## Kimler Kullanır?
* Evrendeki bütün (10 dan fazla makine idaresine sahip) İleri Düzey **Linux Sunucu Yöneticileri, DevSecOps Uzmanları** ve **Bulut Dağıtım (Deploy) Mühendisleri**.
* Müşterilerine "Teslimat Dağıtımı (CI/CD Sürekli Eğitim Hatlarında)" yapan Kurumsal firmalar. (Örn: Sen Github'a Kodunu yolladığında (Push), Jenkins/GitlabCI Gidip Arkaplanda ANSIBLE'ı tetikler... Ansible Gidip Şirketin Gerçek sunucusuna Sizin Kodu Klonlayıp Enjekte eder!) Devasa, Harika Ve Red Hat İmparatorluğunun En güçlü İsviçre Çakısıdır.
