# Vyper

## Özet
Vyper; 2017 yılında Ethereum ağı için (Vitalik Buterin'in de katkılarıyla) geliştirilen, Solidity'nin karmaşık ve hataya açık Nesne Yönelimli (Karmaşık Class/Inheritence vb.) yapısını reddederek, **Python'a Tıpatıp benzeyen sözdizimi ile Sadece En Üst Düzey GÜVENLİK (Security) ve Okunabilirlik** vizyonuyla inşa edilmiş, Ethereum Sanal Makinesi'nde (EVM) çalışan deneysel ve Katı bir Akıllı Kontrat dilidir.

## Nedir ve Ne İşe Yarar?
Solidity mükemmeldi ama tehlikeliydi. Yazılımcılar Solidity ile bir Fonksiyon yazıp onu "Başka Bir Kontrattan Miras Alıp (Inheritance)", Polymorphism içine soktuklarında Kodun gerçekte Zeki Bir Kullanıcı (Hacker) tarafından nasıl Sömürüleceğini Okuyamıyordu(Obfuscation Belası). The DAO Hack (50 Milyon Dolarlık ether Çalınması) gibi olaylar Solidity'nin esnekliğinden Kaynaklanmıştı.

Vyper Dediki: Akıllı Kontratlar (Milyon Dolar Tutan Programlar) Öyle Nesne Yönelimli, Şatafatlı, Bilinmeyen Kod Gölgeleri Olamaz!
Mottosu: **"Okuyucunun Kodu Anlaması (Siber Güvenliği), Yazarın Kodu Yazmasından Çok Daha Önemlidir!"** 
Miras alma (Inheritance) YASAKLANDI. Sonsuz Döngüler (Limitsiz While loop-Gas Tüketim Bugları) YASAKLANDI. Method OVerloading YASAKLANDI. Dil Bilerek Fakirleştrilidiki; Hata Çıkmasın!

**Ne İşe Yarar?**
* **Yüksek Riskli Finansal Protokoller (Yenilmez DeFi'lar):** Sadece "Matematiğin ve Güvenliğin (Auditability)" 1. Sırada Olduğu Devasa Para Havuzları (Örn: Curve Finance Protocol, Milyarlarca Dolar Hacim taşır ve Bizzat Vyper ile Yazılmıştır) Bunu kullanır.
* **Akıllı Kontrat Denetimi (Smart Contract Auditing):** Denetimciler(Siber Güvenlikçiler) Vyper kodunu Cok Rahat Okur Çünkü Arkadan dolasan hiçbir "Sihirli Solidity Modifieri" oyunu yoktur. Kod Dümdüz Daktilo şablonudur.

## Dilin Mantığı ve Kod Yapısı
Tamamen Python (Boşluklara Duyarlı) Sintaksıdır. Python gibi Temiz, Güvenilir, ve Spagettiyi önleyen "Zen Of Vyper" kafasıdır.
Solidity'deki Süslü Karışık Kilitleri `def bagis_yap():` Şeklinde dizeleyen Sadedir.

### Örnek Bir Vyper Kodu: Pythonik Bağış Kontratı (Ekranda Mükkemmel Okunabilirlik!)
Bir Önceki Solidity Dosyasında Yazdığımız O "GizliKumbara" Kodunu; Vyper ile Tertemiz Python Tarzı Güvenlik Hiyerarşisine Döktüğümüzde:

```vyper
# BU BIR VYPER (Akilli Kontrat) KODUDUR (.vy Uzantili)

# 1. DEGISKENLER (Durum-State / Blockchain hafizasi)
# Degiskenlerin TİPİ Yaninda Verilir (Python Type-Hinting Mimrisi):
owner: public(address)       # Sahiplik Adresi (Gorunur)
toplam_bagis: public(uint256) # Topam Biriken Ether (Gorunur)

# 2. KURUCU (INIT) FONKSIYONU (Solidity'deki Constructor)
@external
def __init__():
    # Kontrati Kuran Adami Owner(Sahiop) At!
    self.owner = msg.sender  


# 3. PARA YATIRMA FUNKSIYONU (PAYABLE - Decorator İle)
@external   # Disaridan (Cuzdandan / Müşteriden) Cagirilsin
@payable    # Iceirsine ETHER Koymaya(Gondermi) Izin Verifyorum:
def bagis_yap():
    
    # Assert = Require/Emin Olma Kontrolu (Sifirdan Buyuk Mu Para?)
    assert msg.value > 0, "Lutfen Cimrilik Yapmayin, Ether atin!"
    
    # Kasanin Bakiyesine(Para) Ekleyelin:
    self.toplam_bagis += msg.value


# 4. PARAYI CEKME FOKSIYONU (Pusula Cekimi)
@external
def kumbarayi_patlat():
    
    # 1. GUVENLIK DUVARI : Cagiran Adam BIZIM PATRON (OWNER) MU?!
    assert msg.sender == self.owner, "Dur Orada! Yetkin Yok, Hirsiz!"
    
    # 2. EYLEM : Ceken Patronun Kendi Sahsi Cuzdanina Parayi (Balance) Yolla!
    send(self.owner, self.balance)
    
    # 3. STATU GUNCELLME : Kasayi Sifirla(Kumbariyi kirdik)
    self.toplam_bagis = 0
```
Yukarıdaki kod o kadar anlaşılırdır ki, Yazılımcı olmayan sıradan bir İngilizce Matematik Öğretmeni bile okusa "Hırsızın Parayı Çalamayacağını ve Patronun Çekeneğini" anlar. Vyper'ın En BÜyük Başarısı ve Hack'leri/Dolandırıclıkları önleme Devrimi bu Sadeliktir!

## Kimler Kullanır?
* "Ben Solidity'nin Şaşalı (Fakat Hacklenmeye Müsait) Yapisindan Korkuyorum, Bana En güvenilir ve Sade Kilitli Kutu Motorunu Verin" diyen **Web3 Defi Mimarları / Kriptograflar**.
* Özellikle Akıllı Kontrat geliştirmeye "Python / Veri Bilimcisi" Altyapısından Geçiş Sapan Uzmanlar için Cennetin Cıktığı Kapıdır. Curve Finance gibi Devasa Hacimli Borsaların (DEX) temel Taşı olduğu için Her Zaman Değerli ve Gizli bir Hazine olarak Kalmaya (Solidity Çılgınkalbağılından uzakta) Devam edecektir.
